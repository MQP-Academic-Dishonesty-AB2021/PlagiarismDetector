package com.JavaFX;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.text.SimpleDateFormat;

import static org.slf4j.impl.SimpleLogger.DEFAULT_LOG_LEVEL_KEY;
import static org.slf4j.impl.SimpleLogger.LOG_FILE_KEY;

public class MainMenu extends Application {
	MainMenuController controller;
	private static Parent dashPane;
	private static Stage primaryStage;

	public void loadPartials() throws Exception {
		// Pane dashPane =
		// FXMLLoader.load(MainMenu.class.getResource("/dashboard.fxml"));
	}

	public static Stage getPrimaryStage() {
		return primaryStage;
	}

	private void setPrimaryStage(Stage primaryStage) {
		MainMenu.primaryStage = primaryStage;
	}

	@Override
	public void start(Stage primaryStage) throws Exception {
		String dateString = new SimpleDateFormat("yyyy.MM.dd.hh.mm").format(new java.util.Date());
		System.setProperty(LOG_FILE_KEY, "PlagiarismDetector-" + dateString + ".log");
//		System.setProperty(DEFAULT_LOG_LEVEL_KEY, "error");
		loadPartials();

		FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("/mainMenu.fxml"));
		System.out.println(fxmlLoader.getLocation());
		Parent root = fxmlLoader.load();

		primaryStage.initStyle(StageStyle.DECORATED);

		primaryStage.setScene(new Scene(root, 800, 500));
		primaryStage.show();
	}

	public static void main(String[] args) {
		launch(args);
	}
}
