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
        private Dictionary<string, string> variablesDict = new();
        private Dictionary<Exp.VName, Exp.Expression> intepreterDict = new();
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
        private string DictToString(Dictionary<string, string> d) { return string.Join(Environment.NewLine, d); }
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
                else if ((butt.Background == Brush.Parse("Red")) && (blueBPPassed || startOfDebug))
                {
                    butt.Background = Brush.Parse("Blue");
                    return;
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
            _codeBox.IsReadOnly = false;
            _debugCheck.IsEnabled = true;
            intepreterDict.Clear();
            variablesDict.Clear();
            PaintBlueBreakpointInRed();
        }
        private void RunB_Click(object sender, RoutedEventArgs e)
        {
            int[] getBreakpointIndices(Controls buttons) // Numbers of lines with breakpoints
            {
                var line = 0;
                var bpIndices = new List<int> { 0 };
                foreach (Button butt in _breakpointPanel.Children)
                {
                    if (butt.Background == Brush.Parse("Red")) bpIndices.Add(line);
                    line++;
                }
                return bpIndices.ToArray();
            }
            string arrayToString(string[] arr)  { return String.Join(" ", arr); }
            Queue<string> getSlicesOfCodeByBreakpoints(string code, int[] bpIndices)
            {
                var lines = code.Split(Environment.NewLine);
                var result = new Queue<string> { };
                for (int i = 1; i < bpIndices.Length; i++) result.Enqueue(arrayToString(lines[bpIndices[i-1]..bpIndices[i]]));
                result.Enqueue(arrayToString(lines[bpIndices[^1]..])); // Slice from last breakpoint to the end of code
                return result;
            }
            void continueDebug()
            {
                _runB.IsEnabled = false;
                var currentSlice = debugQueue.Dequeue();
                if (currentSlice.Trim() != "")
                {
                    var task = new Task<Interpreter.Dicts>(() =>
                    {
                        return Interpreter.runVariables(new(variablesDict, intepreterDict), Main.parse(currentSlice));
                    });
                    task.ContinueWith(x =>
                        Dispatcher.UIThread.Post(() =>
                        {
                            try
                            {
                                var res = x.Result;
                                variablesDict = res.VariablesDictionary;
                                intepreterDict = res.InterpretedDictionary;
                                if (variablesDict.Count > 0) _consoleBox.Text = DictToString(variablesDict);
                                _runB.IsEnabled = true;
                            }
                            catch (Exception ex)
                            {
                                LeaveDebugMode(new Object(), new RoutedEventArgs());
                                _consoleBox.Text = ex.Message;
                                _runB.IsEnabled = true;
                            }
                        }));
                    task.Start();
                    PaintNextBreakpoint(false);
                }
                else
                {
                    _runB.IsEnabled = true;
                    PaintNextBreakpoint(false);
                }
            }
            void startDebug()
            {
                _runB.IsEnabled = false;
                var bpIndices = getBreakpointIndices(_breakpointPanel.Children);
                var code = _codeBox.Text;
                var task = new Task<Tuple<Interpreter.Dicts, Queue<string>>>(() =>
                {
                    var debugQueue = getSlicesOfCodeByBreakpoints(code, bpIndices);
                    var currentSlice = debugQueue.Dequeue().Trim();
                    if (currentSlice != "") 
                    {
                        var d = Interpreter.runVariables(new(variablesDict, intepreterDict), Main.parse(currentSlice));
                        return Tuple.Create(d, debugQueue); 
                    }
                    else
                    {
                        return Tuple.Create(new Interpreter.Dicts(variablesDict, intepreterDict), debugQueue);
                    }
                }
                );
                task.ContinueWith(x =>
                    Dispatcher.UIThread.Post(() =>
                    {
                        try
                        {
                            var res = x.Result;           
                            variablesDict = res.Item1.VariablesDictionary;
                            intepreterDict = res.Item1.InterpretedDictionary;
                            if (variablesDict.Count > 0) _consoleBox.Text = DictToString(variablesDict);
                            debugQueue = res.Item2;
                            _runB.IsEnabled = true;
                        }
                        catch (Exception ex)
                        {
                            LeaveDebugMode(new Object(), new RoutedEventArgs());
                            _consoleBox.Text = ex.Message;
                            _runB.IsEnabled = true;
                        }
                    }));
                task.Start();
                EnterDebugMode();
            }
            void executeCode()
            {
                _runB.IsEnabled = false;
                var code = _codeBox.Text;
                var task = new Task<string>(() =>
                {
                    var d = Interpreter.runPrint(Main.parse(code));
                    return d["print"];
                }
                );
                task.ContinueWith(x =>
                    Dispatcher.UIThread.Post(() =>
                    {
                        try
                        {
                            _consoleBox.Text = "Interpretation is finished!" + Environment.NewLine + x.Result;
                            _runB.IsEnabled = true;
                        }
                        catch (Exception ex)
                        {
                            _consoleBox.Text = ex.Message;
                            _runB.IsEnabled = true;
                        }
                    }));
                task.Start();
            }
            _consoleBox.Text = "";
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
                    _runB.IsEnabled = true;
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
