package com.JavaFX;

import Checksims.token.ImmutableToken;
import Comparison.Comparison;
import Comparison.ComparisonPair;
import RacketTree.RacketTree;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.Pane;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;
import javafx.stage.Stage;
import org.apache.commons.lang3.tuple.ImmutablePair;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.ResourceBundle;



public class SetupController implements Initializable {

	double x, y = 0;

	@FXML
	private Pane dashPane;

	@FXML
	private Pane mainPane;

	@FXML
	private ImageView exit;

	DirectoryChooser directoryChooser1 = new DirectoryChooser();
	DirectoryChooser directoryChooser2 = new DirectoryChooser();

	private Stage stage;
	private Scene scene;
	private Parent root;

	//gets folder from dialog

	// Opens dialog to select database of files to be cross referenced
	public void openFileDialogDatabase(ActionEvent event) throws IOException {
		File selectedFile = directoryChooser1.showDialog(((Node) event.getTarget()).getScene().getWindow());
		RacketTree.defaultLeafDepth = 5;
		//maybe detect number of available cores
		Comparison.numThreads = 4;
		Comparison submissions = new Comparison(selectedFile.getAbsolutePath(), Comparison.Method.TreeSimilarity);
		System.out.println();
	}

	// Opens dialog to select database of files to be tested
	public void openFileDialogTested(ActionEvent event) throws IOException {
		File selectedFile2 = directoryChooser1.showDialog(((Node) event.getTarget()).getScene().getWindow());
		Comparison.numThreads = 4;
		Comparison submissions = new Comparison(selectedFile2.getAbsolutePath());
		ArrayList<ImmutablePair<ComparisonPair, Double>> ordered_list = submissions.getOrderedList();

		// Construct csv data:
		StringBuilder dat = new StringBuilder();
		dat.append("group,variable,value");

		for (ImmutablePair<ComparisonPair, Double> comp : ordered_list) {
			String compBase = comp.left.getBaseFile();
			String compCompared = comp.left.getComparedFile();
			double compValue = comp.right;

			System.out.print("Base: " + compBase);
			System.out.print(" Compared: " + compCompared);
			System.out.println(" Value: " + compValue);

			dat.append("\n" + compBase + "," + compCompared + "," + compValue);
		}

		// Write dat to csv:
		try (PrintWriter w = new PrintWriter("matrixdata.csv")) {
			w.write(dat.toString());
			System.out.println("Csv written without errors");
		} catch (FileNotFoundException e) {
			System.out.println(e.getMessage());
		}


	}

	// returns to main menu
	public void returnToMainMenu(ActionEvent event) throws IOException {
		Parent root = FXMLLoader.load(getClass().getResource("/menuBar.fxml"));
		stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
		scene = new Scene(root);
		stage.setScene(scene);
	}

	// returns to main menu
	public void goToResults(ActionEvent event) throws IOException {
		Parent root = FXMLLoader.load(getClass().getResource("/partials/resultsPartial.fxml"));
		stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
		scene = new Scene(root);
		stage.setScene(scene);
	}

	@Override
	@FXML
	public void initialize(URL location, ResourceBundle resources) {

	}
}
