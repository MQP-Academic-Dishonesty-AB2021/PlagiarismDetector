package com.JavaFX;

import RacketTree.RacketTree;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseEvent;
import javafx.scene.text.Text;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import javafx.stage.Stage;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

public class quickstartresultsController implements Initializable {

	@FXML
	private ImageView exit;

	@FXML
	private Text resultValue;

	private Stage stage;
	private Scene scene;
	private Parent root;

	// returns to main menu
	public void returnToMainMenu(MouseEvent event) throws IOException {
		Parent root = FXMLLoader.load(getClass().getResource("/menuBar.fxml"));
		stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
		scene = new Scene(root);
		stage.setScene(scene);
	}

	@Override
	@FXML
	public void initialize(URL location, ResourceBundle resources) {

		// RacketTree racketTree = new RacketTree();
		// resultValue.setText(TreeSimilarity());

	}
}
