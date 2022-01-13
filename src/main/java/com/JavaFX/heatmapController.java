package com.JavaFX;

import com.google.common.collect.ImmutableMap;
import javafx.concurrent.Worker;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Group;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Accordion;
import javafx.scene.control.Button;
import javafx.scene.control.TitledPane;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.web.*;
import javafx.stage.Stage;
import org.apache.velocity.texen.util.FileUtil;
import org.w3c.dom.Document;

import java.awt.*;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Map;
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






	public heatmapController() throws IOException {
	}

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
		String url = getClass().getResource("/matrixVis/matrix.html").getPath();
		String url2 = "www.google.com";



		webEngine.getLoadWorker().stateProperty().addListener((observable, oldState, newState) -> {
			if (newState == Worker.State.SUCCEEDED) {
				Document doc = webEngine.getDocument();
			}
		});
		webEngine.load(url2);



	}
}
