using System;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;
using System.Xml;
using System.Linq;
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
        private readonly Button _stopB;
        private readonly Button _runB;
        private bool debugWarningShowed = false;
        private int debugLine = 0;
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
        private void EnterDebugMode()
        {
            _stopB.IsEnabled = true;
            _runB.Content = "Continue";
            _debugCheck.IsEnabled = false;
        }
        private void LeaveDebugMode(object sender, RoutedEventArgs e)
        {
            debugLine = 0;
            _stopB.IsEnabled = false;
            _runB.Content = "Run";
            _debugCheck.IsEnabled = true;
            debugWarningShowed = false;
            intepreterDict.Clear();
            variablesDict.Clear();
            PaintBlueBreakpointInRed();
        }
        private void RunB_Click(object sender, RoutedEventArgs e)
        {
            string getNextSliceOfCode(string code)
            {
                string arrayToString(string[] a) { return String.Join(" ", a.Select(line => line.Trim())); }
                var line = 0;
                var codeArray = code.Split(Environment.NewLine);
                foreach (Button butt in _breakpointPanel.Children)
                {
                    if (line == debugLine && butt.Background == Brush.Parse("Blue"))
                    {
                        butt.Background = Brush.Parse("Red");
                    }
                    else if (line > debugLine && butt.Background == Brush.Parse("Red"))
                    {
                        butt.Background = Brush.Parse("Blue");
                        var slice = arrayToString(codeArray[debugLine..line]);
                        debugLine = line;
                        return slice.Trim();
                    }
                    line++;
                }
                var lastSlice = arrayToString(codeArray[debugLine..]);
                debugLine = -1; // If this happens, the returned slice is the last slice of code to be executed and debug must stop
                return lastSlice.Trim();
            }
            void executeDebug()
            {
                _runB.IsEnabled = false;
                if (debugLine == 0) EnterDebugMode();
                var slice = getNextSliceOfCode(_codeBox.Text);
                _consoleBox.Text = "Please wait for finish...\n";
                var task = new Task<Interpreter.Dicts>(() =>
                {
                    if (slice != "") 
                    {
                        var d = Interpreter.runVariables(new(variablesDict, intepreterDict), Main.parse(slice));
                        return d; 
                    }
                    else
                    {
                        return new Interpreter.Dicts(variablesDict, intepreterDict);
                    }
                }
                );
                task.ContinueWith(x =>
                    Dispatcher.UIThread.Post(() =>
                    {
                        try
                        {
                            var res = x.Result;           
                            variablesDict = res.VariablesDictionary;
                            intepreterDict = res.InterpretedDictionary;
                            _consoleBox.Text += DictToString(variablesDict) + "\nStep of debug is finished!\n";
                            debugWarningShowed = false;
                            if (debugLine == -1) LeaveDebugMode(new Object(), new RoutedEventArgs());
                        }
                        catch (Exception ex)
                        {
                            LeaveDebugMode(new Object(), new RoutedEventArgs());
                            _consoleBox.Text = ex.Message;
                        }
                        _runB.IsEnabled = true;
                    }));
                task.Start();
            }
            void executeCode()
            {
                _runB.IsEnabled = false;
                _debugCheck.IsEnabled = false;
                var code = _codeBox.Text;
                _consoleBox.Text = "Interpretation is started! Please wait for finish...\n";
                var task = new Task<string>(() =>
                {
                    var d = Interpreter.runPrint(Main.parse(code));
                    return d["print"];
                }
                );
                task.ContinueWith(x =>
                    Dispatcher.UIThread.Post(() =>
                    {
                        try { _consoleBox.Text += x.Result + "Interpretation is finished!\n"; }
                        catch (Exception ex) { _consoleBox.Text = ex.Message; }
                        _runB.IsEnabled = true;
                        _debugCheck.IsEnabled = true;
                    }));
                task.Start();
            }
            _consoleBox.Text = "";
            if (_codeBox.Text.Trim() != "")
            {
                try
                {
                    if (_debugCheck.IsChecked == true) executeDebug(); // Don't kill me, this is null check
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
            ofd.Filters.Add(new FileDialogFilter() { Name = "Txt", Extensions = { "txt" } });
            var path = await ofd.ShowAsync(this);
            if (path != null && path.Length > 0)
            {
                _codeBox.Text = File.ReadAllText(path[0]);
                openedFilePath = path[0];
            }
        }
        public void CodeBox_TextChanged(object sender, EventArgs e)
        {
            var lines = _codeBox.LineCount;
            int childrens = _breakpointPanel.Children.Count;
            if (!_runB.IsEnabled)
            {
                if (_debugCheck.IsChecked == false) _consoleBox.Text = "Interpretation is started! Please wait for finish...\nWarning: Code changes applied after start of execution will not be detected!\n";
                else if (_codeBox.TextArea.Caret.Line <= debugLine || debugLine == -1) _consoleBox.Text = "Please wait for finish...\nWarning: Code changes applied after start of execution will not be detected!\n";
            }
            else if (_codeBox.TextArea.Caret.Line <= debugLine && !debugWarningShowed)
            {
                _consoleBox.Text += "Warning: New code added before current breakpoint will not be executed!\n";
                debugWarningShowed = true;
            }
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
