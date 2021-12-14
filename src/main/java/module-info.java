module com.plagiarismdetector {
	requires javafx.controls;
	requires javafx.fxml;

	requires com.jfoenix;
	requires java.desktop;
	requires javafx.web;
	requires commons.cli;
	requires slf4j.api;
	requires commons.collections4;
	requires commons.io;
	requires slf4j.simple;
	requires guava;
	requires commons.lang3;
	requires reflections;
	requires commons.codec;
	requires velocity;

	opens com.JavaFX to javafx.fxml;

	exports com.JavaFX;
}
