/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 07/apr/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.merge;

import java.io.IOException;

import javafx.fxml.FXMLLoader;
import javafx.geometry.Pos;
import javafx.scene.Group;
import javafx.scene.Node;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.pdfsam.module.ModulePriority;
import org.pdfsam.module.PdfsamModule;
import org.pdfsam.support.io.FileType;
import org.pdfsam.ui.io.BrowsableFileField;
import org.pdfsam.ui.io.PdfDestinationPane;
import org.pdfsam.ui.module.BaseTaskExecutionModule;
import org.pdfsam.ui.selection.SelectionPane;
import org.pdfsam.ui.support.Views;
import org.sejda.model.parameter.MergeParameters;
import org.sejda.model.parameter.base.TaskParameters;
import org.springframework.core.io.ClassPathResource;

/**
 * Merge module to let the user merge together multiple pdf documents
 * 
 * @author Andrea Vacondio
 *
 */
@PdfsamModule
public class MergeModule extends BaseTaskExecutionModule {

    private static final String MERGE_MODULE_ID = "merge";

    private SelectionPane selectionPane;

    public MergeModule() {
        this.selectionPane = new SelectionPane(id());
    }

    @Override
    public ModuleDescriptor descriptor() {
        return new ModuleDescriptor(ModuleCategory.MERGE, DefaultI18nContext.getInstance().i18n("Merge"),
                DefaultI18nContext.getInstance().i18n("Merge together multiple pdf documents or subsections of them."),
                ModulePriority.HIGH.getPriority());
    }

    @Override
    protected TaskParameters getParameters() {
        return new MergeParameters();
    }

    @Override
    protected Pane getInnerPanel() {
        VBox pane = new VBox(5);
        pane.setAlignment(Pos.TOP_CENTER);
        VBox.setVgrow(selectionPane, Priority.ALWAYS);
        BrowsableFileField destination = new BrowsableFileField();
        destination.setFileType(FileType.PDF);
        destination.setMustExist(false);
        PdfDestinationPane destinationPane = new PdfDestinationPane(destination, id());
        pane.getChildren().addAll(selectionPane,
                Views.titledPane(DefaultI18nContext.getInstance().i18n("Destination file"), destinationPane));
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
