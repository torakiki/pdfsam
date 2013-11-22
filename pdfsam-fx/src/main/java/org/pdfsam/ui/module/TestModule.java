/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/nov/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.module;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.IOException;

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXMLLoader;
import javafx.scene.Group;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;

import org.pdfsam.gui.event.AddPdfVersionConstraintEvent;
import org.pdfsam.gui.event.ChangedSelectedPdfVersionEvent;
import org.pdfsam.gui.event.RemovePdfVersionConstraintEvent;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.pdfsam.module.PdfsamModule;
import org.pdfsam.ui.io.PdfVersionCombo;
import org.sejda.model.parameter.base.TaskParameters;
import org.sejda.model.pdf.PdfVersion;
import org.springframework.core.io.ClassPathResource;

/**
 * @author Andrea Vacondio
 * 
 */
@PdfsamModule
public class TestModule extends BaseTaskExecutionModule {

    private static final String MERGE_MODULE_ID = "merge";

    @Override
    public ModuleDescriptor descriptor() {
        return new ModuleDescriptor(ModuleCategory.MERGE, "Merge");
    }

    @Override
    protected TaskParameters getParameters() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    protected Pane getInnerPanel() {
        HBox pane = new HBox();
        Button button = new Button("ADD");
        button.setOnAction(new EventHandler<ActionEvent>() {
            public void handle(ActionEvent event) {
                eventStudio().broadcast(new AddPdfVersionConstraintEvent(PdfVersion.VERSION_1_4), id());
            }
        });
        Button remove = new Button("REMOVE");
        remove.setOnAction(new EventHandler<ActionEvent>() {
            public void handle(ActionEvent event) {
                eventStudio().broadcast(new RemovePdfVersionConstraintEvent(PdfVersion.VERSION_1_4), id());
            }
        });
        Button selected = new Button("SELECTED");
        selected.setOnAction(new EventHandler<ActionEvent>() {
            public void handle(ActionEvent event) {
                eventStudio().broadcast(new ChangedSelectedPdfVersionEvent(PdfVersion.VERSION_1_4), id());
            }
        });
        PdfVersionCombo combo = new PdfVersionCombo(id());
        pane.getChildren().addAll(button, remove, selected, combo);
        return pane;
    }

    @Override
    public String id() {
        return MERGE_MODULE_ID;
    }

    public Node graphic() {
        try {
            return (Group) FXMLLoader.load(new ClassPathResource("/fxml/TestModule.fxml").getURL());
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }
}
