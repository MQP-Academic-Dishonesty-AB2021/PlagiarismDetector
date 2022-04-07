package com.JavaFX;

import javafx.application.Platform;
import javafx.beans.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.binding.NumberBinding;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.ProgressBar;
import javafx.stage.Stage;

import java.net.URL;
import java.util.ResourceBundle;

public class LoadingBarController implements Initializable {
    @FXML
    private ProgressBar bar;

    private IntegerProperty completed;
    private IntegerProperty expected = new SimpleIntegerProperty();

    public void bindCompletion(ObservableValue<? extends Number> binded) {
        this.bar.progressProperty().bind(binded);
    }

    public ProgressBar getProgressBar() {
        return this.bar;
    }

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        this.bar.progressProperty().addListener((obs, oldVal, newVal) -> {
            if (newVal.intValue() == 1) {
                Platform.runLater(() -> {
                    Stage stage = (Stage)bar.getScene().getWindow();
                    stage.close();
                });
            }
        });
    }
}
