module com.plagiarismdetector {
	requires javafx.controls;
	requires javafx.fxml;

	requires com.jfoenix;
	requires java.desktop;
	requires javafx.web;
	requires commons.cli;
	requires commons.collections4;
	requires commons.io;
	requires guava;
	requires commons.lang3;
	requires commons.codec;
	requires velocity;
	requires slf4j.api;
	requires reflections;
	requires slf4j.simple;

	opens com.JavaFX to javafx.fxml;

	exports com.JavaFX;
}
