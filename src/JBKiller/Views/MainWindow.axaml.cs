using System;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;
using System.Xml;
using Avalonia;
using Avalonia.Controls;
using Avalonia.Interactivity;
using Avalonia.Markup.Xaml;
using Avalonia.Media;
using Avalonia.Threading;
using AvaloniaEdit;
using AvaloniaEdit.Highlighting;
using AvaloniaEdit.Highlighting.Xshd;
using CalculatorFSharp;

namespace JBKiller.Views
{
    public class MainWindow : Window
    {
        private readonly TextEditor _codeBox;
        private readonly TextBox _consoleBox;
        private readonly StackPanel _breakpointPanel;
        private readonly CheckBox _debugCheck;
        private Queue<string> debugQueue;
        private readonly Button _stopB;
        private readonly Button _runB;
        private string openedFilePath;
        public MainWindow()
        {
            InitializeComponent();

            _consoleBox = this.Find<TextBox>("ConsoleBox");
            _codeBox = this.FindControl<TextEditor>("CodeBox");
            _breakpointPanel = this.Find<StackPanel>("BreakpointPanel");
            _debugCheck = this.FindControl<CheckBox>("DebugCheck");
            _stopB = this.FindControl<Button>("StopB");
            _runB = this.FindControl<Button>("RunB");

            _stopB.IsEnabled = false;
            _codeBox.TextChanged += CodeBox_TextChanged;
            _codeBox.Background = Brushes.Transparent;
            _codeBox.ShowLineNumbers = true;

            // syntax highlighting
            using (StreamReader s =
            new StreamReader(Directory.GetParent(Environment.CurrentDirectory).Parent.Parent.FullName + "/ArithmHighlighting.xshd"))
            {
                using (XmlTextReader reader = new XmlTextReader(s))
                {
                    _codeBox.SyntaxHighlighting = HighlightingLoader.Load(reader, HighlightingManager.Instance);
                }
            }
        }
        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }
        private string DictToString(Dictionary<string, string> d)
        {
            var res = "";
            foreach (string key in d.Keys) res += key + " = " + d[key] + "\n";
            return res;
        }
        private void PaintBlueBreakpointInRed()
        {
            foreach (Button butt in _breakpointPanel.Children)
            {
                if (butt.Background == Brush.Parse("Blue"))
                {
                    butt.Background = Brush.Parse("Red");
                    break;
                }
            }
        }
        private void PaintNextBreakpoint(bool startOfDebug)
        {
            var blueBPPassed = false;
            foreach (Button butt in _breakpointPanel.Children)
            {
                if (butt.Background == Brush.Parse("Blue"))
                {
                    butt.Background = Brush.Parse("Red");
                    blueBPPassed = true;
                }
                else if (butt.Background == Brush.Parse("Red"))
                {
                    if (blueBPPassed)
                    {
                        butt.Background = Brush.Parse("Blue");
                        return;
                    }
                    else if (startOfDebug)
                    {
                        butt.Background = Brush.Parse("Blue");
                        return;
                    }
                }
            }
            PaintBlueBreakpointInRed(); // If debug is finished
        }
        private void EnterDebugMode()
        {
            _stopB.IsEnabled = true;
            _runB.Content = "Continue";
            _codeBox.IsReadOnly = true;
            _debugCheck.IsEnabled = false;
            PaintNextBreakpoint(true);
        }
        private void LeaveDebugMode(object sender, RoutedEventArgs e)
        {
            debugQueue = null;
            _stopB.IsEnabled = false;
            _runB.Content = "Run";
            _runB.IsEnabled = true;
            _codeBox.IsReadOnly = false;
            _debugCheck.IsEnabled = true;
            PaintBlueBreakpointInRed();
        }
        private void RunB_Click(object sender, RoutedEventArgs e)
        {
            int[] getBreakpointIndices(Controls buttons)
            {
                var line = 0;
                var bpIndices = new List<int> { };
                foreach (Button butt in _breakpointPanel.Children)
                {
                    if (butt.Background == Brush.Parse("Red")) bpIndices.Add(line);
                    line++;
                }
                return bpIndices.ToArray();
            }
            string arrayToString(string[] arr)
            {
                var res = "";
                foreach (string s in arr) if (s.Length > 0) res += s + " ";
                return res;
            }
            Queue<string> getSlicesOfCodeByBreakpoints(string code, int[] bpIndices)
            {
                var lines = code.Split(Environment.NewLine);
                var result = new Queue<string> { };
                for (int i = 0; i < bpIndices.Length; i++) result.Enqueue(arrayToString(lines[0..bpIndices[i]]));
                result.Enqueue(code);
                return result;
            }
            void continueDebug()
            {
                _runB.IsEnabled = false;
                var currentSlice = debugQueue.Dequeue();
                if (currentSlice.Trim() != "")
                {
                    var task = new Task<string>(() =>
                    {
                        var d = Interpreter.run(Main.parse(currentSlice));
                        return DictToString(d.Item2);
                    });
                    task.ContinueWith(x =>
                        Dispatcher.UIThread.Post(() =>
                        {
                            try
                            {
                                if (x.Result != null) _consoleBox.Text = x.Result;
                                _runB.IsEnabled = true;
                            }
                            catch (Exception ex)
                            {
                                LeaveDebugMode(new Object(), new RoutedEventArgs());
                                _consoleBox.Text = ex.Message;
                            }
                        }));
                    task.Start();
                    PaintNextBreakpoint(false);
                }
            }
            void startDebug()
            {
                var bpIndices = getBreakpointIndices(_breakpointPanel.Children);
                var code = _codeBox.Text;
                var task = new Task<Tuple<string, Queue<string>>>(() =>
                {
                    var debugQueue = getSlicesOfCodeByBreakpoints(code, bpIndices);
                    var d = Interpreter.run(Main.parse(debugQueue.Dequeue()));
                    return Tuple.Create(DictToString(d.Item2), debugQueue);
                }
                );
                task.ContinueWith(x =>
                    Dispatcher.UIThread.Post(() =>
                    {
                        try
                        {
                            var resString = x.Result.Item1;
                            if (resString != null) _consoleBox.Text = resString;
                            debugQueue = x.Result.Item2;
                        }
                        catch (Exception ex)
                        {
                            LeaveDebugMode(new Object(), new RoutedEventArgs());
                            _consoleBox.Text = ex.Message;
                        }
                    }));
                task.Start();
                EnterDebugMode();
            }
            void executeCode()
            {
                var code = _codeBox.Text;
                var task = new Task<string>(() =>
                {
                    var d = Interpreter.run(Main.parse(code));
                    return d.Item3["print"];
                }
                );
                task.ContinueWith(x =>
                    Dispatcher.UIThread.Post(() =>
                    {
                        try
                        {
                            if (x.Result != null) _consoleBox.Text = x.Result;
                        }
                        catch (Exception ex)
                        {
                            _consoleBox.Text = ex.Message;
                        }
                    }));
                task.Start();
            }
            if (_codeBox.Text.Trim() != "")
            {
                try
                {
                    if (debugQueue != null) continueDebug();
                    else if (_debugCheck.IsChecked == true) startDebug(); // Don't kill me, this is null check
                    else executeCode();
                }
                catch (Exception ex)
                {
                    LeaveDebugMode(new Object(), new RoutedEventArgs());
                    _consoleBox.Text = ex.Message;
                }
            }
        }
        async private void SaveB_Click(object sender, RoutedEventArgs e)
        {
            var sfd = new SaveFileDialog();
            sfd.InitialFileName = openedFilePath;
            var path = await sfd.ShowAsync(this);
            if (path != null) File.WriteAllText(path, _codeBox.Text);
        }
        async private void LoadB_Click(object sender, RoutedEventArgs e)
        {
            var ofd = new OpenFileDialog();
            var path = await ofd.ShowAsync(this);
            openedFilePath = path[0];
            if (path.Length > 0) _codeBox.Text = File.ReadAllText(path[0]);
        }
        public void CodeBox_TextChanged(object sender, EventArgs e)
        {
            var lines = _codeBox.LineCount;
            int childrens = _breakpointPanel.Children.Count;
            if (childrens > lines)
            {
                _breakpointPanel.Children.RemoveRange(lines, childrens - lines);
            }
            else if (childrens < lines)
            {
                for (var i = childrens; i < lines; i++)
                {
                    var button = new Button()
                    {
                        Height = 16.6,
                        Width = 20,
                        Background = Brush.Parse("Green"),
                        Margin = Thickness.Parse("0,0,0,0"),
                        BorderThickness = Thickness.Parse("0,0,0,0")
                    };
                    if (i > 0) button.Click += breakPoint_Click;
                    void breakPoint_Click(object sender, RoutedEventArgs e)
                    {
                        if (debugQueue == null)
                        {
                            if (button.Background == Brush.Parse("Green"))
                            {
                                button.Background = Brush.Parse("Red");
                            }
                            else
                            {
                                button.Background = Brush.Parse("Green");
                            }
                        }
                    }
                    _breakpointPanel.Children.Add(button);
                }
            }
        }
    }
}
