/*
 * This file is part of the PDF Split And Merge source code
 * Created on 27/nov/2013
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
package org.pdfsam.ui.components.selection.multiple;

import java.io.*;
import java.net.*;
import java.nio.file.*;
import java.util.Collections;
import javafx.scene.control.*;
import javafx.scene.input.*;
import javafx.scene.layout.VBox;

class MockFileExplorer extends VBox {

    MockFileExplorer(Path tempFolder) throws URISyntaxException, IOException {
        var fileList = new ListView<File>();

        fileList.setId("file-list");
        fileList.setCellFactory(param -> new FileListCell());
        fileList.setOnDragDetected(event -> onDragDetected(event, fileList));
        fileList.setOnDragOver(this::onDragOver);

        var pdfs = new File[]{
            Files.createFile(tempFolder.resolve("1.pdf")).toFile(),
            Files.createFile(tempFolder.resolve("2.pdf")).toFile()
        };

        fileList.getItems().addAll(pdfs);

        getChildren().add(fileList);
    }

    private void onDragDetected(MouseEvent event, ListView<File> fileList) {
        var selectedIndex = fileList.getSelectionModel().getSelectedIndex();
        
        if (selectedIndex >= 0) {
            var file = fileList.getItems().get(selectedIndex);
            var dragboard = fileList.startDragAndDrop(TransferMode.COPY);
            var content = new ClipboardContent();
            
            content.put(DataFormat.FILES, Collections.singletonList(file));
            dragboard.setContent(content);

            event.consume();
        }
    }

    private void onDragOver(DragEvent event) {
        var dragboard = event.getDragboard();
        
        if (dragboard.hasFiles()) {
            event.acceptTransferModes(TransferMode.COPY);
        }
    }

    private static class FileListCell extends ListCell<File> {

        @Override
        protected void updateItem(File item, boolean empty) {
            super.updateItem(item, empty);

            if (empty) {
                setText(null);
            } else {
                setText(item.getName());
            }
        }
    }

}
