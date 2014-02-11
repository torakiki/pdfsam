/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20/nov/2013
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

import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.ui.banner.BannerPane;
import org.pdfsam.ui.menu.AppMenuBar;

/**
 * Main panel containing menu, banner and the content area
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class MainPane extends VBox {

    @Inject
    private ContentPane mainPane;
    @Inject
    private AppMenuBar menuBar;
    @Inject
    private BannerPane banner;

    @PostConstruct
    public void init() {
        VBox.setVgrow(mainPane, Priority.ALWAYS);
        getChildren().addAll(menuBar, banner, mainPane);
    }
}
