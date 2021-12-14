package com.JavaFX;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import javafx.stage.StageStyle;



public class MainMenu extends Application {
    double x,y = 0;
    MenuController controller;
    private static Parent dashPane;



    public void loadPartials() throws Exception {
        //Pane dashPane = FXMLLoader.load(MainMenu.class.getResource("/dashboard.fxml"));
    }

    @Override
    public void start(Stage primaryStage) throws Exception{
        loadPartials();




        FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("/menuBar.fxml"));
        System.out.println(fxmlLoader.getLocation());
        Parent root = fxmlLoader.load();

        primaryStage.initStyle(StageStyle.UNDECORATED);

        root.setOnMousePressed(event -> {
            x = event.getSceneX();
            y = event.getSceneY();
        });

        root.setOnMouseDragged(event -> {
            primaryStage.setX(event.getScreenX() - x);
            primaryStage.setY(event.getScreenY() - y);
        });


        primaryStage.setScene(new Scene(root, 800, 500));
        primaryStage.show();
    }




    public static void main(String[] args) {
        launch(args);
    }
}
