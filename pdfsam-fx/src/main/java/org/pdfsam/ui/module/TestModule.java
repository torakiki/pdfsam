/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/nov/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.module;

import java.io.IOException;

import javafx.fxml.FXMLLoader;
import javafx.scene.Group;
import javafx.scene.Node;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.pdfsam.module.PdfsamModule;
import org.pdfsam.ui.io.BrowsableDirectoryField;
import org.pdfsam.ui.io.PdfDestinationPane;
import org.pdfsam.ui.selection.SelectionPane;
import org.pdfsam.ui.support.Style;
import org.pdfsam.ui.support.Views;
import org.sejda.model.parameter.base.TaskParameters;
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
        return new ModuleDescriptor(ModuleCategory.MERGE, "Merge", "Merge pdf documents together", -1);
    }

    @Override
    protected TaskParameters getParameters() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    protected Pane getInnerPanel() {
        BorderPane pane = new BorderPane();
        pane.getStyleClass().addAll(Style.CONTAINER.css());
        BrowsableDirectoryField destination = new BrowsableDirectoryField();
        PdfDestinationPane destinationPane = new PdfDestinationPane(destination, id());
        pane.setCenter(new SelectionPane(id()));
        pane.setBottom(Views.titledPane(DefaultI18nContext.getInstance().i18n("Destination folder"), destinationPane));
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
