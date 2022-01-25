package com.JavaFX;

import Comparison.Comparison;
import Comparison.ComparisonPair;
import RacketTree.RacketTree;
import com.jfoenix.controls.JFXSlider;
import javafx.event.ActionEvent;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TreeTableColumn;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TreeItemPropertyValueFactory;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.text.Text;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import javafx.stage.Stage;
import org.apache.commons.lang3.tuple.ImmutablePair;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.ResourceBundle;

public class quickstartresultsController implements Initializable {

	@FXML
	private ImageView exit;

	@FXML
	private Text resultValue;

	private Stage stage;
	private Scene scene;
	private Parent root;

	@FXML
	public TableView tableView;

	@FXML
	private JFXSlider leafSlider;

	@FXML
	private JFXSlider threadSlider;

	@FXML
	private TableColumn assignmentACol;

	@FXML
	private TableColumn assignmentBCol;

	@FXML
	private Button loadButton;

	@FXML
	private TableColumn nodeCol;

	// returns to main menu
	public void returnToMainMenu(MouseEvent event) throws IOException {
		Parent root = FXMLLoader.load(getClass().getResource("/menuBar.fxml"));
		stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
		scene = new Scene(root);
		stage.setScene(scene);
	}

	// returns to table results
	public void returnToTable(MouseEvent event) throws IOException {
		Parent root = FXMLLoader.load(getClass().getResource("/resultsPartial.fxml"));
		stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
		scene = new Scene(root);
		stage.setScene(scene);
	}

	// opens detail view of a case
	public void openDetailView(MouseEvent event) throws IOException {
		if(tableView.getSelectionModel().getSelectedItem() != null) {
			Parent root = FXMLLoader.load(getClass().getResource("/tableResultsPartial.fxml"));
			stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
			scene = new Scene(root);
			stage.setScene(scene);
		}
	}

	/*@FXML
	private void sendDataDetailView(ActionEvent event, Case aCase) {
		// Step 1
		Case aDetailCase = aCase;
		// Step 2
		Node node = (Node) event.getSource();
		// Step 3
		Stage stage = (Stage) node.getScene().getWindow();
		stage.close();
		try {
			// Step 4
			Parent root = FXMLLoader.load(getClass().getResource("/resultsPartial.fxml"));
			// Step 5
			stage.setUserData(aCase);
			// Step 6
			Scene scene = new Scene(root);
			stage.setScene(scene);
			// Step 7
			stage.show();
		} catch (IOException e) {
			System.err.println(String.format("Error: %s", e.getMessage()));
		}
	}*/

	public void runComparison(Comparison submissions) throws IOException {

	}


	@FXML
	private void receiveData(MouseEvent event) {
		// Step 1
		Node node = (Node) event.getSource();
		Stage stage = (Stage) node.getScene().getWindow();
		// Step 2
		Case aCase = (Case) stage.getUserData();
		// Step 3
		ArrayList<Case> ordered_list = aCase.listOfCases;
		for (int i = 0; i < ordered_list.size(); i++) {
			Case aCase2 = aCase.listOfCases.get(i);
			Case assignmentA = aCase.listOfCases.get(i);
			Case assignmentB = aCase.listOfCases.get(i);
			Case val = aCase.listOfCases.get(i);

			assignmentACol.setCellValueFactory(
					new PropertyValueFactory<>("fileA")
			);

			assignmentBCol.setCellValueFactory(
					new PropertyValueFactory<>("fileB")
			);

			nodeCol.setCellValueFactory(
					new PropertyValueFactory<>("val")
			);

			tableView.getItems().addAll(aCase.listOfCases);
		}

	}


	@Override
	@FXML
	public void initialize(URL location, ResourceBundle resources) {




	}
}
