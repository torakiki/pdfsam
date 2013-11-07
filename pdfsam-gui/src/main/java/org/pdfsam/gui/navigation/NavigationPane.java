/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/nov/2013
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
package org.pdfsam.gui.navigation;

import javafx.scene.layout.VBox;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.module.UsageService;
import org.pdfsam.ui.support.Style;

/**
 * Vertical panel holding the quick navigation icons
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class NavigationPane extends VBox {

    @Inject
    private UsageService usage;
    @Inject
    private ExpandButton expandButton;
    @Inject
    private MostUsedModulesPane mostUsedModulesPane;

    public NavigationPane() {
        getStyleClass().addAll(Style.NAVIGATION_BAR.css());
    }

    @PostConstruct
    private void init() {
        usage.getMostRecentlyUsed();
        mostUsedModulesPane.displayTextProperty().bind(expandButton.selectedProperty());
        getChildren().addAll(expandButton, mostUsedModulesPane);
    }
}
