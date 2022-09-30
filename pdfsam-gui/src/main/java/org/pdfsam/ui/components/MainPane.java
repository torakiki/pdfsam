/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20/nov/2013
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
package org.pdfsam.ui.components;

import javax.inject.Inject;

import org.pdfsam.injector.Auto;
import org.pdfsam.ui.components.banner.BannerPane;

import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;

/**
 * Main panel containing menu, banner and the content area
 * 
 * @author Andrea Vacondio
 * 
 */
@Auto
public class MainPane extends VBox {

    @Inject
    public MainPane(ContentPane mainPane, BannerPane banner) {
        VBox.setVgrow(mainPane, Priority.ALWAYS);
        this.setId("pdfsam-main-pane");
        getChildren().addAll(banner, mainPane);
    }
}
