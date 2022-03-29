package com.JavaFX;

import Comparison.Comparison;
import Comparison.ComparisonPair;
import com.jfoenix.controls.JFXSlider;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseEvent;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.ImmutableTriple;

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
		Comparison comparisonData = (Comparison) stage.getUserData();
		ArrayList<ImmutablePair<ComparisonPair, Double>> pairs = comparisonData.getOrderedList();
		assignmentACol.setCellValueFactory(
				new PropertyValueFactory<>("left")
		);

		assignmentBCol.setCellValueFactory(
				new PropertyValueFactory<>("middle")
		);

		nodeCol.setCellValueFactory(
				new PropertyValueFactory<>("right")
		);
		for (ImmutablePair<ComparisonPair, Double> pair : pairs) {
			tableView.getItems().add(new ImmutableTriple<String, String, Double>(pair.left.getBaseFile(), pair.left.getComparedFile(), pair.right));
		}
//		for (ImmutablePair<ComparisonPair, Double> pair : comparisons) {
//			pair.left.
//			Case aCase2 = aCase.listOfCases.get(i);
//			Case assignmentA = aCase.listOfCases.get(i);
//			Case assignmentB = aCase.listOfCases.get(i);
//			Case val = aCase.listOfCases.get(i);
//
//
//		}

	}


	@Override
	@FXML
	public void initialize(URL location, ResourceBundle resources) {
		assignmentACol.setCellValueFactory(
				new PropertyValueFactory<>("left")
		);

		assignmentBCol.setCellValueFactory(
				new PropertyValueFactory<>("middle")
		);

		nodeCol.setCellValueFactory(
				new PropertyValueFactory<>("right")
		);

		tableView.sceneProperty().addListener(((observableValue, oldScene, newScene) -> {
			if (oldScene == null && newScene != null) {
				newScene.windowProperty().addListener(((observableWindow, oldWindow, newWindow) -> {
					Stage stage = (Stage) newWindow;
					Comparison comparisonData = (Comparison) stage.getUserData();
					ArrayList<ImmutablePair<ComparisonPair, Double>> pairs = comparisonData.getOrderedList();
					for (ImmutablePair<ComparisonPair, Double> pair : pairs) {
						tableView.getItems().add(new ImmutableTriple<String, String, Double>(pair.left.getBaseFile(), pair.left.getComparedFile(), pair.right));
					}
				}));
			}
		}));
	}
}
