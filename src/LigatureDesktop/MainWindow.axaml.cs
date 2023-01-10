// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

using Avalonia.Controls;
using Avalonia.Interactivity;
using System;
using Avalonia.Logging;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using static Ligature;
using static LigatureInMemory;

namespace LigatureDesktop;

public partial class MainWindow : Window
{
    private LigatureInMemory.LigatureInMemory ligature = new LigatureInMemory.LigatureInMemory(null);
    private ObservableCollection<string> datasets = new ObservableCollection<string>();
    const string noDatasets = "No Datasets";
    public MainWindow()
    {
        InitializeComponent();
        DatasetComboBox.Items = datasets;
        refreshDatasets();
        DatasetComboBox.SelectedIndex = 0;
    }

    private void refreshDatasets()
    {
        datasets.Add(noDatasets);
    }

    private void AddDatasetClick(object sender, RoutedEventArgs e)
    {
	    var datasetName = this.NewDatasetName.Text;
        if (datasets[0] == noDatasets) {
            datasets[0] = datasetName;
            DatasetComboBox.SelectedIndex = 0;
        } else {
            datasets.Add(datasetName);
        }
        this.AddDatasetButton.Flyout.Hide();
        this.NewDatasetName.Text = "";
    }

    private void RemoveDatasetYes(object sender, RoutedEventArgs e)
    {
	    //do something on click
    }

    private void RemoveDatasetNo(object sender, RoutedEventArgs e)
    {
	    //do something on click
    }

    private void RefreshDatasetClick(object sender, RoutedEventArgs e)
    {
	    //do something on click
    }

    private void RunClick(object sender, RoutedEventArgs e)
    {
	    //do something on click
    }

    private void ClearClick(object sender, RoutedEventArgs e)
    {
	    //do something on click
    }
}
