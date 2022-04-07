package com.JavaFX;

import Comparison.Comparison;
import Comparison.ComparisonPromise;
import RacketTree.RacketTree;
import com.jfoenix.controls.JFXButton;
import com.jfoenix.controls.JFXCheckBox;
import com.jfoenix.controls.JFXSlider;
import javafx.beans.binding.NumberBinding;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.TextField;
import javafx.scene.image.ImageView;
import javafx.scene.layout.Pane;
import javafx.stage.DirectoryChooser;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

public class SetupController implements Initializable {
	public static Logger logger = LoggerFactory.getLogger(SetupController.class);
	double x, y = 0;

	@FXML
	private Pane dashPane;

	@FXML
	private Pane mainPane;

	@FXML
	private ImageView exit;

	@FXML
	private JFXSlider leafSlider;

	@FXML
	private JFXSlider threadSlider;

	@FXML
	private JFXCheckBox useChecksims;

	@FXML
	private JFXButton doneButton;

	@FXML
	private TextField TestingPath;


	DirectoryChooser directoryChooser1 = new DirectoryChooser();
	DirectoryChooser directoryChooser2 = new DirectoryChooser();

	private Stage stage;
	private Scene scene;
	private Parent root;

	public File databaseDirectory;
	public File testingDirectory;

	// Opens dialog to select database of files to be cross referenced
	public void openFileDialogDatabase(ActionEvent event) throws IOException {
		File selectedFile = directoryChooser1.showDialog(((Node) event.getTarget()).getScene().getWindow());
		databaseDirectory = selectedFile;
	}

	// Opens dialog to select database of files to be tested
	public void openFileDialogTested(ActionEvent event) throws IOException {
		File selectedFile = directoryChooser1.showDialog(((Node) event.getTarget()).getScene().getWindow());
		if (selectedFile == null) { return; }
		TestingPath.textProperty().set(selectedFile.getAbsolutePath());
	}

	// returns to main menu

	public void returnToMainMenu(ActionEvent event) throws IOException {
		Parent root = FXMLLoader.load(getClass().getResource("/mainMenu.fxml"));
		stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
		scene = new Scene(root);
		stage.setScene(scene);
	}

	@FXML
	private void sendData(ActionEvent event) {
		Comparison.numThreads = (int)Math.round(threadSlider.getValue());
		RacketTree.defaultLeafDepth = (int)Math.round(leafSlider.getValue());
		Comparison.Method method = useChecksims.selectedProperty().getValue() ?
				Comparison.Method.Checksims :
				Comparison.Method.TreeSimilarity;
		ComparisonPromise promise = new ComparisonPromise(testingDirectory.getAbsolutePath(), method);
		promise.start();

		try {
			FXMLLoader progressLoader = new FXMLLoader(getClass().getResource("/loadingPane.fxml"));
			Parent root = progressLoader.load();
			LoadingBarController barController = progressLoader.getController();
			barController.bindCompletion(promise.getPercentCompletion());
			NumberBinding test = promise.getPercentCompletion();
			test.addListener(new ChangeListener<Number>() {
				@Override
				public void changed(ObservableValue<? extends Number> observableValue, Number number, Number t1) {
					System.out.println(t1);
				}
			});
			Stage stage = new Stage();
			stage.initOwner(((Node)event.getSource()).getScene().getWindow());
			stage.initModality(Modality.APPLICATION_MODAL);
			stage.setResizable(false);
			stage.initStyle(StageStyle.UTILITY);
			Scene scene = new Scene(root);
			stage.setScene(scene);
			stage.setAlwaysOnTop(true);
			stage.showAndWait();
		}
		catch (IOException e) {
			logger.error(e.getMessage());
			e.printStackTrace();
		}
		try {
			promise.join();
		}
		catch (InterruptedException e){
			return;
		}

		Comparison results = null;
		try {
			results = promise.getComparison();
		}
		catch (InterruptedException e) {

		}
		if (results == null) { return; }
		// Step 2
		Node node = (Node) event.getSource();
		// Step 3
		Stage stage = (Stage) node.getScene().getWindow();
		try {
			// Step 4
//			Parent root = FXMLLoader.load(getClass().getResource("/resultsPartial.fxml"));
			FXMLLoader resultsLoader = new FXMLLoader(getClass().getResource("/resultsPartial.fxml"));
			Parent root = resultsLoader.load();
			ResultsController resultsController = resultsLoader.getController();
			// Step 5
			stage.setUserData(results);
			// Step 6
//			Scene scene = new Scene(root);
			stage.getScene().setRoot(root);
			resultsController.receiveData();
			// Step 7
		} catch (IOException e) {
			System.err.println(String.format("Error: %s", e.getMessage()));
		}
	}

	// returns to main menu
	public void goToResults(ActionEvent event) throws IOException {
		Parent root = FXMLLoader.load(getClass().getResource("/resultsPartial.fxml"));
		stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
		scene = new Scene(root);
		stage.setScene(scene);
	}

	public void detectAvailableCores() throws IOException {
		int cores = Runtime.getRuntime().availableProcessors();
		threadSlider.setValue(cores);
	}

	@FXML
	public void PathChanged(Event e) {
		testingDirectory = new File(TestingPath.getText());
		doneButton.setDisable(!(testingDirectory.isDirectory() && testingDirectory.exists()));
	}

	@Override
	@FXML
	public void initialize(URL location, ResourceBundle resources) {
		try {
			detectAvailableCores();
		}
		catch(IOException e) {
			e.printStackTrace();
		}
	}
}
