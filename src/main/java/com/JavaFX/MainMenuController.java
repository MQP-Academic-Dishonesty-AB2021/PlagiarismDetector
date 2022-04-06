package com.JavaFX;

import com.jfoenix.controls.JFXButton;
import javafx.animation.FadeTransition;
import javafx.animation.TranslateTransition;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.control.Label;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Pane;
import javafx.stage.Stage;
import javafx.util.Duration;

import javafx.event.ActionEvent;

import java.awt.*;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ResourceBundle;

public class MainMenuController implements Initializable {

	@FXML
	private AnchorPane opacityPane;

	@FXML
	private GridPane drawerPane;

	@FXML
	private Label drawerImage;

	@FXML
	private JFXButton quickStartButton;

	@FXML
	private JFXButton visualizationsText;

	public boolean firstTimeSetup;
	public void switchToSetup(ActionEvent event) throws IOException {
		if (!firstTimeSetup) {
			Parent root = FXMLLoader.load(getClass().getResource("/setup.fxml"));
			Stage stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
			stage.getScene().setRoot(root);
			firstTimeSetup = true;
		} else {
			// switchToResults(event);
		}
	}

	public void switchToSettings(ActionEvent event) throws IOException {
		if (firstTimeSetup == false) {
			Parent root = FXMLLoader.load(getClass().getResource("/settings.fxml"));
			Stage stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
			stage.getScene().setRoot(root);
			firstTimeSetup = true;
		} else {
			// switchToResults(event);
		}
	}

	public void switchToVisualizations(ActionEvent event) throws IOException {
		Parent root = FXMLLoader.load(getClass().getResource("/partials/heatmapPartial.fxml"));
		Stage stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
		stage.getScene().setRoot(root);
		firstTimeSetup = true;
	}

	public void openDefaultBrowser(ActionEvent event) throws IOException {
		try {
			Desktop.getDesktop().browse(new URI("http://localhost:63342/PD1214/PlagarismDetector/matrixVis/matrix.html?sha=0&shb=0"));
		} catch (IOException e1) {
			e1.printStackTrace();
		} catch (URISyntaxException e1) {
			e1.printStackTrace();
		}
	}


	@FXML
	public Pane mainPane;


	@Override
	@FXML
	public void initialize(URL location, ResourceBundle resources) {
		opacityPane.setVisible(false);

		FadeTransition fadeTransition = new FadeTransition(Duration.seconds(0.5), opacityPane);
		fadeTransition.setFromValue(1);
		fadeTransition.setToValue(0);
		fadeTransition.play();

		TranslateTransition translateTransition = new TranslateTransition(Duration.seconds(0.5), drawerPane);
		translateTransition.setByX(-600);
		translateTransition.play();

		drawerImage.setOnMouseClicked(event -> {

			opacityPane.setVisible(true);

			FadeTransition fadeTransition1 = new FadeTransition(Duration.seconds(0.5), opacityPane);
			fadeTransition1.setFromValue(0);
			fadeTransition1.setToValue(0.15);
			fadeTransition1.play();

			TranslateTransition translateTransition1 = new TranslateTransition(Duration.seconds(0.5), drawerPane);
			translateTransition1.setByX(+600);
			translateTransition1.play();
		});

		opacityPane.setOnMouseClicked(event -> {

			FadeTransition fadeTransition1 = new FadeTransition(Duration.seconds(0.5), opacityPane);
			fadeTransition1.setFromValue(0.15);
			fadeTransition1.setToValue(0);
			fadeTransition1.play();

			fadeTransition1.setOnFinished(event1 -> {
				opacityPane.setVisible(false);
			});

			TranslateTransition translateTransition1 = new TranslateTransition(Duration.seconds(0.5), drawerPane);
			translateTransition1.setByX(-600);
			translateTransition1.play();
		});

	}
}
