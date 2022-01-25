package com.JavaFX;

import Comparison.Comparison;
import Comparison.ComparisonPair;
import org.apache.commons.lang3.tuple.ImmutablePair;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class Case {

    String fileA;
    String fileB;
    double val;
    ArrayList<Case> listOfCases;
    boolean checkBox;

    public Case(String fileA, String fileB, double val) {
        this.fileA = fileA;
        this.fileB = fileB;
        this.val = val;

    }

    public Case(ArrayList listOfCases) {
        this.listOfCases = listOfCases;

    }



    public String getFileA() {
        return fileA;
    }

    public String getFileB() {
        return fileB;
    }

    public double getVal() {
        return val;
    }

    public static ArrayList createListOfCases(File selectedFile2, boolean checkBox) {
        if(checkBox){
            Comparison submissions = new Comparison(selectedFile2.getAbsolutePath(), Comparison.Method.Checksims);

            ArrayList<ImmutablePair<ComparisonPair, Double>> ordered_list = submissions.getOrderedList();
            ArrayList<Case> storedCases = new ArrayList<Case>();
            for (ImmutablePair<ComparisonPair, Double> comp : ordered_list) {
                Case aCase = new Case(comp.left.getBaseFile(), comp.left.getComparedFile(), comp.right);
                storedCases.add(aCase);
            }
            return storedCases;
        }
        else {
            Comparison submissions = new Comparison(selectedFile2.getAbsolutePath());

            ArrayList<ImmutablePair<ComparisonPair, Double>> ordered_list = submissions.getOrderedList();
            ArrayList<Case> storedCases = new ArrayList<Case>();
            for (ImmutablePair<ComparisonPair, Double> comp : ordered_list) {
                Case aCase = new Case(comp.left.getBaseFile(), comp.left.getComparedFile(), comp.right);
                storedCases.add(aCase);
            }
            return storedCases;
        }
    }
}


