using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using Avalonia;
using Avalonia.Controls;
using Avalonia.Interactivity;
using Avalonia.Markup.Xaml;
using Avalonia.Media;
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
        public MainWindow()
        {
            InitializeComponent();

            _consoleBox = this.Find<TextBox>("ConsoleBox");
            _codeBox = this.FindControl<TextEditor>("CodeBox");
            _breakpointPanel = this.Find<StackPanel>("BreakpointPanel");
            _debugCheck = this.FindControl<CheckBox>("DebugCheck");

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
                    res += s[..(s.Length - 1)] + " ";
                }
                return res;
            }
            Queue<string> getSlicesOfCodeByBreakpoints(string code, int[] bpIndices)
            {
                var lines = code.Split("\n");
                var result = new Queue<string> { };
                for (int i = 0; i < bpIndices.Length; i++)
                {
                    result.Enqueue(arrayToString(lines[0..bpIndices[i]])); //////////
                }
                result.Enqueue(code);
                return result;
            }
            try
            {
                if (_debugCheck.IsChecked == true) // Не бейте, он иначе на нулл ругается
                {
                    var bpIndices = getBreakpointIndices(_breakpointPanel.Children);
                    debugQueue = getSlicesOfCodeByBreakpoints(_codeBox.Text, bpIndices);
                    var d = Interpreter.run(Main.parse(debugQueue.Dequeue()));
                    var x = dictToString(d.Item2);
                    if (x != null) _consoleBox.Text = x;
                }
                else
                {
                    var d = Interpreter.run(Main.parse(_codeBox.Text));
                    var x = d.Item3["print"];
                    if (x != null) _consoleBox.Text = x;
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

                if (_debugCheck.IsChecked == true)
                {
                    var d = Interpreter.run(Main.parse(debugQueue.Dequeue()));
                    var x = dictToString(d.Item2);
                    if (x != null) _consoleBox.Text = x;
                }
                else
                {
                    var d = Interpreter.run(Main.parse(_codeBox.Text));
                    var x = d.Item3["print"];
                    if (x != null) _consoleBox.Text = x;
                }
            }
            catch (Exception ex)
            {
                _consoleBox.Text = ex.Message;
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
