package com.JavaFX;

import Comparison.Comparison;
import Comparison.ComparisonPair;
import RacketTree.RacketSubmission;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.collections.ObservableList;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.control.TableCell;
import javafx.scene.control.TextArea;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.image.ImageView;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.ImmutableTriple;
import org.apache.commons.lang3.tuple.Triple;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.ResourceBundle;

public class DetailedViewController implements Initializable {
    private static Logger logger = LoggerFactory.getLogger(DetailedViewController.class);

    private Comparison comparison;
    private ObservableList<ImmutableTriple<RacketSubmission, RacketSubmission, Double>> values;
    private IntegerProperty currentIndex;

    @FXML
    private Text baseFilename, comparedFilename;

    @FXML
    private Text caseNum;

    @FXML
    private Text resultsValue;

    @FXML
    private TextArea baseFileText, comparedFileText;

    @FXML
    private ImageView previousButton, nextButton;

    @FXML
    private void previousEntry(Event e) {
        this.currentIndex.setValue(this.currentIndex.getValue() - 1);
    }

    @FXML
    private void nextEntry(Event e) {
        this.currentIndex.setValue(this.currentIndex.getValue() + 1);
    }

    @FXML
    private void back(Event event) {
        Node node = (Node) event.getSource();
        Stage stage = (Stage) node.getScene().getWindow();
        try {
            FXMLLoader resultsLoader = new FXMLLoader(getClass().getResource("/resultsPartial.fxml"));
            Parent root = resultsLoader.load();
            ResultsController resultsController = resultsLoader.getController();
            stage.setUserData(this.comparison);
            stage.getScene().setRoot(root);
            resultsController.receiveData();
        }
        catch (IOException e) {
            logger.error(String.format("Error: %s", e.getMessage()));
        }
    }

    public void receiveData() {
        Stage stage = (Stage) resultsValue.getScene().getWindow();
        Triple<Comparison, ObservableList, Integer> tuple =
                (Triple<Comparison, ObservableList, Integer>) stage.getUserData();
        this.comparison = tuple.getLeft();
        this.values = tuple.getMiddle();
        this.currentIndex.set(tuple.getRight());
    }

    private void setSubmission(ImmutableTriple<RacketSubmission, RacketSubmission, Double> submission) {
        RacketSubmission base = submission.getLeft();
        RacketSubmission compared = submission.getMiddle();
        Double val = submission.getRight();


        baseFilename.setText(base.getName());
        comparedFilename.setText(compared.getName());

        resultsValue.setText(String.format("%.2f", val));

        try {
            StringBuilder baseText = new StringBuilder();
            Files.walk(base.regular().toPath())
                    .filter(Files::isRegularFile)
                    .forEach(p -> {
                        try {
                            baseText.append(Files.readString(p));
                        } catch (IOException e) {}
                    });
            if (baseText.length() == 0) {
                baseFileText.setText("Error Loading File");
            }
            else {
                baseFileText.setText(baseText.toString());
            }
        } catch (IOException e) {
            baseFileText.setText("Error Loading File");
        }
        try {
            StringBuilder comparedText = new StringBuilder();
            Files.walk(compared.regular().toPath())
                    .filter(Files::isRegularFile)
                    .forEach(p -> {
                        try {
                            comparedText.append(Files.readString(p));
                        } catch (IOException e) {}
                    });
            if (comparedText.length() == 0) {
                comparedFileText.setText("Error Loading File");
            }
            else {
                comparedFileText.setText(comparedText.toString());
            }
        } catch (IOException e) {
            comparedFileText.setText("Error Loading File");
        }
    }

    @Override
    @FXML
    public void initialize(URL location, ResourceBundle resources) {
        this.currentIndex = new SimpleIntegerProperty(-1);
        this.currentIndex.addListener((o, oldVal, newVal) -> {
            this.caseNum.setText("Case " + Integer.toString(newVal.intValue()));
            this.setSubmission(this.values.get(newVal.intValue()));
            this.previousButton.setDisable(newVal.intValue() <= 0);
            this.nextButton.setDisable(newVal.intValue() >= this.values.size() - 1);
        });
        caseNum.sceneProperty().addListener(((observableValue, oldScene, newScene) -> {
            if (oldScene == null && newScene != null) {
                newScene.windowProperty().addListener(((observableWindow, oldWindow, newWindow) -> {
                    receiveData();
                }));
            }
        }));
    }
}
