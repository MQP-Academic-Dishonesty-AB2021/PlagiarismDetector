package com.JavaFX;

import javafx.concurrent.Worker;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.web.*;
import javafx.stage.Stage;
import org.w3c.dom.Document;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

public class heatmapController implements Initializable {

	@FXML
	private ImageView exit;

	@FXML
	private AnchorPane webPane;

	@FXML
	private WebView webview;
	private WebEngine engine;

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

		WebView webView = new WebView();
		WebEngine webEngine = webView.getEngine();
		String url = getClass().getResource("/matrixVis/fileview.html").toExternalForm();
		webEngine.getLoadWorker().stateProperty().addListener((observable, oldState, newState) -> {
			if (newState == Worker.State.SUCCEEDED) {
				Document doc = webEngine.getDocument();
			}
		});
		webEngine.loadContent(url);

	}
}
