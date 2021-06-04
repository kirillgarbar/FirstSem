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

namespace AvaloniaEditDemo.Views
{
    public class MainWindow : Window
    {
        private readonly TextEditor _codeBox;
        private readonly TextBox _consoleBox;
        private readonly StackPanel _breakpointPanel;
        private readonly CheckBox _debugCheck;
        private Queue<string> debugQueue;
        private readonly Button _continueB;
        public MainWindow()
        {
            InitializeComponent();

            _consoleBox = this.Find<TextBox>("ConsoleBox");
            _codeBox = this.FindControl<TextEditor>("CodeBox");
            _breakpointPanel = this.Find<StackPanel>("BreakpointPanel");
            _debugCheck = this.FindControl<CheckBox>("DebugCheck");
            _continueB = this.FindControl<Button>("ContinueB");

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
        private string dictToString(Dictionary<string, string> d)
        {
            var res = "";
            foreach (string key in d.Keys) res += key + " = " + d[key] + "\n";
            return res;
        }
        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
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
                foreach (string s in arr)
                {
                    if (s.Length > 0) res += s + " ";
                }
                return res;
            }
            Queue<string> getSlicesOfCodeByBreakpoints(string code, int[] bpIndices)
            {
                var lines = code.Split(Environment.NewLine);
                var result = new Queue<string> { };
                for (int i = 0; i < bpIndices.Length; i++)
                {
                    result.Enqueue(arrayToString(lines[0..bpIndices[i]]));
                }
                result.Enqueue(code);
                return result;
            }
            try
            {
                if (_debugCheck.IsChecked == true) // Не бейте, он иначе на нулл ругается
                {
                    var bpIndices = getBreakpointIndices(_breakpointPanel.Children);
                    var code = _codeBox.Text;
                    var task = new Task<Tuple<string, Queue<string>>>(() =>    
                    {
                        var debugQueue = getSlicesOfCodeByBreakpoints(code, bpIndices);
                        var d = Interpreter.run(Main.parse(debugQueue.Dequeue()));
                        return Tuple.Create(dictToString(d.Item2), debugQueue);
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
                                _consoleBox.Text = ex.Message;
                            }
                        }));
                    task.Start();                    
                }
                else
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
            }
            catch (Exception ex)
            {
                _consoleBox.Text = ex.Message;
            }
        }
        private void ContinueB_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                if (_debugCheck.IsChecked == true && debugQueue != null)
                {
                    _continueB.IsEnabled = false;
                    var currentSlice = debugQueue.Dequeue();
                    var task = new Task<string>(() =>
                    {
                        var d = Interpreter.run(Main.parse(currentSlice));
                        return dictToString(d.Item2);
                    }
                    );
                    task.ContinueWith(x =>
                        Dispatcher.UIThread.Post(() =>
                        {
                            if (x.Result != null) _consoleBox.Text = x.Result;
                            _continueB.IsEnabled = true;
                        }));
                    task.Start();
                }
            }
            catch (Exception ex)
            {
                _consoleBox.Text = ex.Message;
                _continueB.IsEnabled = true;
            }
        }
        async private void SaveB_Click(object sender, RoutedEventArgs e)
        {
            var sfd = new SaveFileDialog();
            var path = await sfd.ShowAsync(this);
            if (path != null) File.WriteAllText(path, _codeBox.Text);
        }
        async private void LoadB_Click(object sender, RoutedEventArgs e)
        {
            var ofd = new OpenFileDialog();
            var path = await ofd.ShowAsync(this);
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
                        if (button.Background == Brush.Parse("Green"))
                        {
                            button.Background = Brush.Parse("Red");
                        }
                        else
                        {
                            button.Background = Brush.Parse("Green");
                        }
                    }
                    _breakpointPanel.Children.Add(button);
                }
            }

        }
    }
}
