/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/ott/2013
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
package org.pdfsam.ui.io;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.stage.DirectoryChooser;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.support.validation.Validators;

/**
 * Component letting the user select an existing directory
 * 
 * @author Andrea Vacondio
 * 
 */
public class BrowsableDirectoryField extends BrowsableField {

    public BrowsableDirectoryField() {
        setBrowseWindowTitle(DefaultI18nContext.getInstance().i18n("Select a directory"));
        getTextField().setValidator(Validators.newExistingDirectoryString());
        getTextField().setErrorMessage(DefaultI18nContext.getInstance().i18n("Select an existing directory"));
        getTextField().setPromptText(DefaultI18nContext.getInstance().i18n("Select a directory"));
        getBrowseButton().setOnAction(new BrowseEventHandler());
    }

    /**
     * {@link EventHandler} opening the {@link DirectoryChooser} and letting the user select the input directory populating the ValidableTextField.
     * 
     * @author Andrea Vacondio
     * 
     */
    private class BrowseEventHandler implements EventHandler<ActionEvent> {

        public void handle(ActionEvent event) {
            DirectoryChooser fileChooser = FileChoosers.getDirectoryChooser(getBrowseWindowTitle());
            String currentSelection = getTextField().getText();
            if (isNotBlank(currentSelection)) {
                Path path = Paths.get(currentSelection);
                if (Files.exists(path)) {
                    fileChooser.setInitialDirectory(path.toFile());
                }
            }
            File chosenFile = fileChooser.showDialog(getTextField().getScene().getWindow());
            if (chosenFile != null) {
                getTextField().setText(chosenFile.getAbsolutePath());
                getTextField().validate();
            }
        }
    }
}
