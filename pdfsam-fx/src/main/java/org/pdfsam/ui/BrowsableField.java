/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.ui;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

import java.io.File;

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.Button;
import javafx.scene.layout.BorderPane;
import javafx.stage.FileChooser;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.io.FileChoosers;
import org.pdfsam.support.io.FileType;
import org.pdfsam.support.validation.Validators;

/**
 * {@link ValidableTextField} with attached a browse button to let the user select a file
 * 
 * @author Andrea Vacondio
 * 
 */
public class BrowsableField extends BorderPane {
    private Button browseButton;
    private ValidableTextField textField = new ValidableTextField();
    private FileType fileType = FileType.ALL;
    private String browseWindowTitle = DefaultI18nContext.getInstance().i18n("Select a file");

    public BrowsableField() {
        browseButton = new Button(DefaultI18nContext.getInstance().i18n("Browse"));
        browseButton.setOnAction(new BrowseEventHandler());
        browseButton.getStyleClass().addAll("pdfsam-button", "browse-button");
        browseButton.setMinHeight(USE_PREF_SIZE);
        browseButton.setMaxHeight(USE_PREF_SIZE);
        browseButton.prefHeightProperty().bind(textField.heightProperty());
        setCenter(textField);
        setRight(browseButton);
    }

    public void setFileType(FileType fileType) {
        this.fileType = fileType;
        textField.setValidator(Validators.newFileTypeString(fileType));
        textField.setErrorMessage(DefaultI18nContext.getInstance().i18n("Allowed extensions are {0}",
                fileType.getFilter().getExtensions().toString()));
    }

    /**
     * @return the internal {@link ValidableTextField}
     */
    public ValidableTextField getTextField() {
        return textField;
    }

    /**
     * {@link EventHandler} opening the {@link FileChooser} and letting the user select the input file/directory populating the {@link ValidableTextField}.
     * 
     * @author Andrea Vacondio
     * 
     */
    private class BrowseEventHandler implements EventHandler<ActionEvent> {

        public void handle(ActionEvent event) {
            FileChooser fileChooser = FileChoosers.getFileChooser(fileType, browseWindowTitle);
            String currentSelection = textField.getText();
            if (isNotBlank(currentSelection)) {
                File currentFile = new File(currentSelection);
                if (currentFile.isFile()) {
                    fileChooser.setInitialDirectory(currentFile.getParentFile());
                    fileChooser.setInitialFileName(currentFile.getName());
                }
            }
            File chosenFile = fileChooser.showOpenDialog(BrowsableField.this.getScene().getWindow());
            if (chosenFile != null) {
                textField.setText(chosenFile.getAbsolutePath());
                textField.validate();
            }
        }

    }

}
