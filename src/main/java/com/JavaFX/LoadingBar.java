package com.JavaFX;

import javafx.beans.binding.NumberBinding;
import javafx.beans.property.DoubleProperty;
import javafx.beans.property.IntegerProperty;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;

public class LoadingBar {
    public static Logger logger = LoggerFactory.getLogger(LoadingBar.class);

    LoadingBar(Window owner, DoubleProperty numFinished, DoubleProperty numExpected) throws LoadingCancelledError {
        try {
            FXMLLoader progressLoader = new FXMLLoader(getClass().getResource("/loadingPane.fxml"));
            Parent root = progressLoader.load();
            LoadingBarController barController = progressLoader.getController();
            barController.bindCompletion(numFinished, numExpected);
            Stage stage = new Stage();
            stage.initOwner(owner);
            stage.initModality(Modality.APPLICATION_MODAL);
            stage.setResizable(false);
            stage.initStyle(StageStyle.UTILITY);
            stage.setOnCloseRequest((event) -> barController.cancel(null));
            Scene scene = new Scene(root);
            stage.setScene(scene);
            stage.setAlwaysOnTop(true);
            stage.showAndWait();
            if (barController.wasCancelled()) {
                throw new LoadingCancelledError();
            }
        }
		catch (IOException e) {
            logger.error(e.getMessage());
            e.printStackTrace();
        }
    }
}
