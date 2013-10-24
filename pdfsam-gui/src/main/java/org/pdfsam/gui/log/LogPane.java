/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 17/ott/2013
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
package org.pdfsam.gui.log;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.pdfsam.support.io.TextFileWriter.writeContent;

import java.io.File;
import java.io.IOException;

import javafx.beans.binding.BooleanBinding;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.control.TextArea;
import javafx.scene.layout.VBox;
import javafx.stage.FileChooser;

import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.bushe.swing.event.annotation.EventSubscriber;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.I18nContext;
import org.pdfsam.gui.support.FileChoosers;
import org.pdfsam.gui.support.FileExtensionFilter;

/**
 * Panel displaying log messages
 * 
 * @author Andrea Vacondio
 * 
 */
public class LogPane extends VBox {
    @FXML
    private TextArea logArea;

    public LogPane() {
        FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("/fxml/LogPane.fxml"));
        fxmlLoader.setRoot(this);
        fxmlLoader.setController(this);

        try {
            fxmlLoader.load();
        } catch (IOException exception) {
            throw new RuntimeException(exception);
        }
        initMenu();
        AnnotationProcessor.process(this);
    }

    private void initMenu() {
        I18nContext i18n = DefaultI18nContext.getInstance();

        MenuItem copyItem = new MenuItem(i18n.i18n("Copy"));
        copyItem.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent action) {
                logArea.copy();
            }
        });
        copyItem.getStyleClass().add("ctx-menu-item");

        // disable if no selection
        copyItem.disableProperty().bind(new BooleanBinding() {
            {
                bind(logArea.selectionProperty());
            }

            @Override
            protected boolean computeValue() {
                return logArea.selectionProperty().getValue().getLength() <= 0;
            }
        });

        MenuItem clearItem = new MenuItem(i18n.i18n("Clear"));
        clearItem.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent action) {
                logArea.clear();
            }
        });
        clearItem.getStyleClass().add("ctx-menu-item");
        // disable if there's no text
        clearItem.disableProperty().bind(new BooleanBinding() {
            {
                bind(logArea.textProperty());
            }

            @Override
            protected boolean computeValue() {
                return isEmpty(logArea.textProperty().getValue());
            }
        });

        MenuItem selectAllItem = new MenuItem(i18n.i18n("Select all"));
        selectAllItem.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent action) {
                logArea.selectAll();
            }
        });
        selectAllItem.getStyleClass().add("ctx-menu-item");
        // disable if there's no text
        selectAllItem.disableProperty().bind(clearItem.disableProperty());

        MenuItem saveItem = new MenuItem(i18n.i18n("Save log"));
        saveItem.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent action) {
                saveLog();
            }
        });
        saveItem.getStyleClass().add("ctx-menu-item");
        // disable if there's no text
        saveItem.disableProperty().bind(clearItem.disableProperty());
        SeparatorMenuItem separator = new SeparatorMenuItem();
        logArea.setContextMenu(new ContextMenu(copyItem, clearItem, selectAllItem, separator, saveItem));
    }

    @EventSubscriber
    public void onLogMessage(LogMessageEvent event) {
        logArea.appendText(String.format("%s %s", event.getLevel(), event.getMessage()));
        if (event.getStack() != null) {
            for (StackTraceElement current : event.getStack()) {
                logArea.appendText(current.toString());
            }
        }
    }

    public void saveLog() {
        FileChooser fileChooser = FileChoosers.getFileChooser(FileExtensionFilter.LOG, DefaultI18nContext.getInstance()
                .i18n("Select where to save the log file"));
        fileChooser.setInitialFileName("PDFsam.log");
        File chosenFile = fileChooser.showSaveDialog(this.getScene().getWindow());
        if (chosenFile != null) {
            if (chosenFile.exists()) {
                // TODO show dialog? investigate. On Ubuntu it already asks confirmation.
            }
            writeContent(logArea.getText()).to(chosenFile);
        }
    }
}
