/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10 ago 2016
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.gui.components.dialog;

import jakarta.inject.Inject;
import jakarta.inject.Named;
import javafx.collections.FXCollections;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.model.tool.ClearToolRequest;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.ui.InputPdfArgumentsLoadRequest;
import org.pdfsam.model.ui.SetActiveToolRequest;
import org.pdfsam.ui.components.commons.HideOnEscapeHandler;
import org.pdfsam.ui.components.support.Style;

import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

import static java.util.Comparator.comparing;
import static java.util.stream.Collectors.toList;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Dialog asking the user which module he wants to use to open one or more PDF files received as input arguments from the application
 * 
 * @author Andrea Vacondio
 */
public class OpenWithDialog extends Stage {

    private Label messageTitle = new Label();
    private HBox buttons = new HBox(5);
    private ListView<String> filesList = new ListView<>();
    private List<Tool> tools;

    @Inject
    public OpenWithDialog(List<Tool> tools, @Named("primaryStage") Stage stage) {
        initModality(Modality.WINDOW_MODAL);
        initStyle(StageStyle.UTILITY);
        setResizable(false);
        setTitle(i18n().tr("Open with"));
        initOwner(stage);

        this.tools = tools.stream().sorted(comparing(m -> m.descriptor().name())).collect(toList());

        messageTitle.getStyleClass().add("-pdfsam-open-with-dialog-title");

        BorderPane containerPane = new BorderPane();
        containerPane.getStyleClass().addAll(Style.CONTAINER.css());
        containerPane.getStyleClass().addAll("-pdfsam-open-with-dialog", "-pdfsam-open-with-container");
        containerPane.setTop(messageTitle);
        BorderPane.setAlignment(messageTitle, Pos.TOP_CENTER);

        filesList.setPrefHeight(150);
        containerPane.setCenter(filesList);

        buttons.getStyleClass().addAll(Style.CONTAINER.css());
        containerPane.setBottom(buttons);
        BorderPane.setAlignment(buttons, Pos.CENTER);

        Scene scene = new Scene(containerPane);
        scene.setOnKeyReleased(new HideOnEscapeHandler(this));
        setScene(scene);
        app().runtimeState().subscribeThemedScene(scene);
        eventStudio().addAnnotatedListeners(this);
        this.setOnShown(e -> requestFocus());
    }

    OpenWithDialog initFor(InputPdfArgumentsLoadRequest event) {

        this.messageTitle.setText(i18n().tr("Select the task to perform on the following files"));
        filesList.setItems(
                FXCollections.observableArrayList(event.pdfs().stream().map(Path::toString).collect(toList())));
        tools.forEach(m -> {
            if (m.descriptor().hasInputType(event.requiredInputType())) {
                Button current = new Button(m.descriptor().name());
                current.getStyleClass().addAll(Style.FOOTER_BUTTON.css());

                Optional.ofNullable(m.graphic()).ifPresent(g -> {
                    g.setScaleX(0.7);
                    g.setScaleY(0.7);
                    current.setGraphic(g);
                });

                current.setOnAction((e) -> {
                    eventStudio().broadcast(new ClearToolRequest(m.id(), false, false), m.id());
                    eventStudio().broadcast(new SetActiveToolRequest(m.id()));
                    hide();
                    PdfLoadRequest loadEvent = new PdfLoadRequest(m.id());
                    event.pdfs().stream().map(Path::toFile).map(PdfDocumentDescriptor::newDescriptorNoPassword)
                            .forEach(loadEvent::add);
                    eventStudio().broadcast(loadEvent, m.id());
                });
                buttons.getChildren().add(current);
            }
        });
        return this;
    }

}
