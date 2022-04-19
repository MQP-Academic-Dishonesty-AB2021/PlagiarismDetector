package com.JavaFX;

import Comparison.Comparison;
import Comparison.ComparisonPair;
import RacketTree.RacketSubmission;
import com.jfoenix.controls.JFXSlider;
import javafx.beans.binding.Bindings;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseEvent;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import javafx.util.Callback;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.ImmutableTriple;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.ResourceBundle;

public class TextResultsController implements Initializable {

	@FXML
	private ImageView exit;

	@FXML
	private Text resultValue;

	@FXML
	private Text assignment1;

	@FXML
	private Text assignment2;

	@FXML
	private Text caseNum;


	@FXML
	private Text possibleCases;

	@FXML
	public TableView tableView;

	@FXML
	private JFXSlider leafSlider;

	@FXML
	private JFXSlider threadSlider;

	@FXML
	private TableColumn<Object, Object> assignmentACol;

	@FXML
	private TableColumn<Object, Object> assignmentBCol;

	@FXML
	private Button loadButton;

	@FXML
	private TableColumn nodeCol;

	// returns to main menu
	public void returnToMainMenu(MouseEvent event) throws IOException {
		Parent root = FXMLLoader.load(getClass().getResource("/mainMenu.fxml"));
		Stage stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
		stage.getScene().setRoot(root);
	}

	// returns to table results
	public void returnToTable(MouseEvent event) throws IOException {
		Parent root = FXMLLoader.load(getClass().getResource("/resultsPartial.fxml"));
		Stage stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
		stage.getScene().setRoot(root);
	}

	// opens detail view of a case
	public void openDetailView(ImmutableTriple<String, String, Double> item) throws IOException {
		if(tableView.getSelectionModel().getSelectedItem() != null) {
			Parent root = FXMLLoader.load(getClass().getResource("/tableResultsPartial.fxml"));
			Stage stage = (Stage) tableView.getScene().getWindow();
			stage.getScene().setRoot(root);
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

	//Highlighter Code

	/*JFXHighlighter highlighter = new JFXHighlighter();
	highlighter.setPaint(Color.YELLOW);

	TextArea textArea = new TextArea("This is my text");
	Button highlightButton = new Button("Highlight");
	VBox parent = new VBox(textArea, highlightButton);

	highlightButton.setOnAction(event -> {
		highlighter.highlight(parent, "text");
		event.consume();
	});*/

	public void runComparison(Comparison submissions) throws IOException {

	}


	@FXML
	public void receiveData() {
		// Step 1
		Stage stage = (Stage) tableView.getScene().getWindow();
		// Step 2
		Comparison comparisonData = (Comparison) stage.getUserData();
		ArrayList<ImmutablePair<ComparisonPair, Double>> pairs = comparisonData.getOrderedList();
		tableView.setRowFactory(new Callback<TableView<ImmutableTriple<String, String, Double>>, TableRow<ImmutableTriple<String, String, Double>>>() {
			@Override
			public TableRow<ImmutableTriple<String, String, Double>> call(TableView<ImmutableTriple<String, String, Double>> view) {
				final TableRow<ImmutableTriple<String, String, Double>>	row = new TableRow<>();
				final ContextMenu rowMenu = new ContextMenu();
				MenuItem detailView = new MenuItem("Open Detailed View");
				detailView.setOnAction(e -> {
					try {
						openDetailView(row.getItem());
					} catch (IOException ex) {
						ex.printStackTrace();
					}
				});
				rowMenu.getItems().add(detailView);
				row.contextMenuProperty().bind(
						Bindings.when(row.emptyProperty()).then((ContextMenu) null).otherwise(rowMenu)
				);
				row.setOnMouseClicked(event -> {
					if (event.getClickCount() == 2 && (!row.isEmpty())) {
						try {
							openDetailView(row.getItem());
						} catch (IOException e) {
							e.printStackTrace();
						}
					}
				});
				return row;
			}
		});
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
			tableView.getItems().add(new ImmutableTriple<RacketSubmission, RacketSubmission, Double>(pair.left.getBaseFile(), pair.left.getComparedFile(), pair.right));
		}

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

		nodeCol.setCellFactory(c -> new TableCell<String, Double>() {
			@Override
			protected void updateItem(Double balance, boolean empty) {
				super.updateItem(balance, empty);
				if (balance == null || empty) {
					setText(null);
				}
				else {
					setText(String.format("%.2f", balance));
				}
			}
		});

		tableView.sceneProperty().addListener(((observableValue, oldScene, newScene) -> {
			if (oldScene == null && newScene != null) {
				newScene.windowProperty().addListener(((observableWindow, oldWindow, newWindow) -> {
					Stage stage = (Stage) newWindow;
					Comparison comparisonData = (Comparison) stage.getUserData();
					ArrayList<ImmutablePair<ComparisonPair, Double>> pairs = comparisonData.getOrderedList();
					for (ImmutablePair<ComparisonPair, Double> pair : pairs) {
						tableView.getItems().add(new ImmutableTriple<RacketSubmission, RacketSubmission, Double>
								(pair.left.getBaseFile(), pair.left.getComparedFile(), pair.right));
					}
				}));
			}
		}));
	}
}
