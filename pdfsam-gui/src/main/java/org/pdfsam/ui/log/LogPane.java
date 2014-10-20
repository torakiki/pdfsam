/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 17/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.ui.log;

import static org.pdfsam.support.io.ObjectCollectionWriter.writeContent;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.Collection;

import javafx.beans.binding.BooleanBinding;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.input.Clipboard;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.layout.BorderPane;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.i18n.I18nContext;
import org.pdfsam.support.io.FileType;
import org.pdfsam.ui.io.FileChoosers;
import org.pdfsam.ui.io.RememberingLatestFileChooserWrapper;
import org.pdfsam.ui.support.Style;

/**
 * Panel displaying log messages
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class LogPane extends BorderPane {

    private LogListView logView;

    @Inject
    public LogPane(LogListView view) {
        this.logView = view;
        getStyleClass().addAll(Style.CONTAINER.css());
        setCenter(this.logView);

        I18nContext i18n = DefaultI18nContext.getInstance();
        MenuItem copyItem = new MenuItem(i18n.i18n("Copy"));
        copyItem.setId("copyLogMenuItem");
        copyItem.setAccelerator(new KeyCodeCombination(KeyCode.C, KeyCombination.SHORTCUT_DOWN));
        copyItem.setOnAction(e -> copyLog(logView.getSelectionModel().getSelectedItems()));

        // disable if no selection
        copyItem.disableProperty().bind(new BooleanBinding() {
            {
                bind(logView.getSelectionModel().getSelectedIndices());
            }

            @Override
            protected boolean computeValue() {
                return logView.getSelectionModel().getSelectedItems().isEmpty();
            }
        });

        MenuItem clearItem = new MenuItem(i18n.i18n("Clear"));
        clearItem.setId("clearLogMenuItem");
        clearItem.setOnAction(e -> logView.getItems().clear());
        // disable if there's no text
        clearItem.disableProperty().bind(new BooleanBinding() {
            {
                bind(logView.getItems());
            }

            @Override
            protected boolean computeValue() {
                return logView.getItems().isEmpty();
            }
        });

        MenuItem selectAllItem = new MenuItem(i18n.i18n("Select all"));
        selectAllItem.setId("selectAllLogMenuItem");
        selectAllItem.setOnAction(e -> logView.getSelectionModel().selectAll());
        // disable if there's no text
        selectAllItem.disableProperty().bind(clearItem.disableProperty());

        MenuItem saveItem = new MenuItem(i18n.i18n("Save log"));
        saveItem.setId("saveLogMenuItem");
        saveItem.setOnAction(e -> saveLog());
        // disable if there's no text
        saveItem.disableProperty().bind(clearItem.disableProperty());
        SeparatorMenuItem separator = new SeparatorMenuItem();
        logView.setContextMenu(new ContextMenu(copyItem, clearItem, selectAllItem, separator, saveItem));
        logView.focusedProperty().addListener(o -> eventStudio().broadcast(new ChangedVisiblityLogAreaEvent()));
    }

    public void saveLog() {
        RememberingLatestFileChooserWrapper fileChooser = FileChoosers.getFileChooser(FileType.LOG, DefaultI18nContext
                .getInstance().i18n("Select where to save the log file"));
        fileChooser.setInitialFileName("PDFsam.log");
        File chosenFile = fileChooser.showSaveDialog(this.getScene().getWindow());
        if (chosenFile != null) {
            if (chosenFile.exists()) {
                // TODO show dialog? investigate. On Ubuntu it already asks confirmation.
            }
            writeContent(logView.getItems()).to(chosenFile);
        }
    }

    public void copyLog(Collection<LogMessage> selected) {
        if (!selected.isEmpty()) {
            ClipboardContent content = new ClipboardContent();
            writeContent(selected).to(content);
            Clipboard.getSystemClipboard().setContent(content);
        }
    }
}
