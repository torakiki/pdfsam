/*
 * This file is part of the PDF Split And Merge source code
 * Created on 17/ott/2013
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.gui.components.content.log;

import jakarta.inject.Inject;
import javafx.beans.binding.Bindings;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.input.Clipboard;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.layout.BorderPane;
import org.pdfsam.core.io.Choosers;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.model.io.FileType;
import org.pdfsam.model.log.ClearLogRequest;
import org.pdfsam.model.log.SaveLogRequest;

import java.util.Collection;
import java.util.Objects;

import static org.pdfsam.core.support.io.ObjectCollectionWriter.writeContent;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Panel displaying log messages
 *
 * @author Andrea Vacondio
 */
public class LogPane extends BorderPane {

    private final LogListView logView;

    @Inject
    public LogPane(LogListView view, LogPaneToolbar toolbar) {
        this.logView = view;
        getStyleClass().addAll("logs-pane", "spaced-container");
        setCenter(this.logView);
        setTop(toolbar);
        MenuItem copyItem = new MenuItem(i18n().tr("Copy"));
        copyItem.setId("copyLogMenuItem");
        copyItem.setAccelerator(new KeyCodeCombination(KeyCode.C, KeyCombination.SHORTCUT_DOWN));
        copyItem.setOnAction(e -> copyLog(logView.getSelectionModel().getSelectedItems()));

        // disable if no selection
        copyItem.disableProperty().bind(Bindings.isEmpty(logView.getSelectionModel().getSelectedItems()));

        MenuItem clearItem = new MenuItem(i18n().tr("Clear"));
        clearItem.setId("clearLogMenuItem");
        clearItem.setOnAction(e -> clearLog(null));
        // disable if there's no text
        clearItem.disableProperty().bind(Bindings.isEmpty(logView.getItems()));

        MenuItem selectAllItem = new MenuItem(i18n().tr("Select all"));
        selectAllItem.setId("selectAllLogMenuItem");
        selectAllItem.setOnAction(e -> logView.getSelectionModel().selectAll());
        // disable if there's no text
        selectAllItem.disableProperty().bind(clearItem.disableProperty());

        MenuItem saveItem = new MenuItem(i18n().tr("Save log"));
        saveItem.setId("saveLogMenuItem");
        saveItem.setOnAction(e -> saveLog(null));
        // disable if there's no text
        saveItem.disableProperty().bind(clearItem.disableProperty());
        logView.setContextMenu(new ContextMenu(copyItem, clearItem, selectAllItem, new SeparatorMenuItem(), saveItem));
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void saveLog(SaveLogRequest request) {
        var fileChooser = Choosers.fileChooser(i18n().tr("Select where to save the log file"), FileType.LOG);
        fileChooser.setInitialFileName("PDFsam.log");
        var saveTo = fileChooser.showSaveDialog(this.getScene().getWindow());
        if (Objects.nonNull(saveTo)) {
            //            if (chosenFile.exists()) {
            //                 TODO show dialog? investigate. On Ubuntu it already asks confirmation.
            //            }
            writeContent(logView.getItems()).to(saveTo);
        }
    }

    @EventListener
    public void clearLog(ClearLogRequest request) {
        logView.getItems().clear();
    }

    public void copyLog(Collection<LogMessage> selected) {
        if (!selected.isEmpty()) {
            ClipboardContent content = new ClipboardContent();
            writeContent(selected).to(content);
            Clipboard.getSystemClipboard().setContent(content);
        }
    }
}
