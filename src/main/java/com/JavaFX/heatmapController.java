package com.JavaFX;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.image.ImageView;
import javafx.scene.layout.Pane;
import javafx.scene.web.*;

import java.net.URL;
import java.util.ResourceBundle;

public class heatmapController implements Initializable {

    @FXML
    private ImageView exit;

    @FXML
    private WebView webview1;
    private WebEngine engine;

    @Override
    @FXML
    public void initialize(URL location, ResourceBundle resources) {
        WebEngine webEngine = webview1.getEngine();
        webEngine.load(getClass().getResource("/matrix.html").toString() );

        exit.setOnMouseClicked(event -> {
            System.exit(0);
        });


    }
}

