package com.JavaFX;

import com.jfoenix.controls.JFXButton;
import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.beans.property.DoubleProperty;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.ProgressBar;
import javafx.scene.input.MouseEvent;
import javafx.scene.text.Text;
import javafx.stage.Stage;

import java.awt.geom.QuadCurve2D;
import java.net.URL;
import java.util.ResourceBundle;

public class LoadingBarController implements Initializable {
    @FXML
    private ProgressBar bar;

    @FXML
    private Text infoText, countText;

    @FXML
    private JFXButton cancelButton;

    private boolean cancelled = false;

    private DoubleProperty numFinished, numExpected;

    public void bindCompletion(DoubleProperty numFinished, DoubleProperty numExpected) {
        this.numFinished.bind(numFinished);
        this.numExpected.bind(numExpected);
        this.bar.progressProperty().bind(Bindings.divide(this.numFinished, this.numExpected));
    }

    public boolean wasCancelled() {
        return this.cancelled;
    }

    private void updateNumber() {
        this.countText.setText(Integer.toString(numFinished.intValue()) +  "/"+ Integer.toString(numExpected.intValue()));
    }

    @FXML
    public void cancel(ActionEvent event) {
        this.cancelled = true;
        Stage stage = (Stage)bar.getScene().getWindow();
        stage.close();
    }

    public ProgressBar getProgressBar() {
        return this.bar;
    }

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        this.numFinished = new SimpleDoubleProperty(0);
        this.numExpected = new SimpleDoubleProperty(1);
        this.bar.progressProperty().addListener((obs, oldVal, newVal) -> {
            if (newVal.intValue() == 1) {
                Platform.runLater(() -> {
                    Stage stage = (Stage)bar.getScene().getWindow();
                    stage.close();
                });
            }
        });
        this.numFinished.addListener((o, oldVal, newVal) -> {
            Platform.runLater(() -> updateNumber());
        });
    }
}
