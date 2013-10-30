/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/ott/2013
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
package org.pdfsam.gui.preference;

import javafx.scene.Node;
import javafx.scene.control.TitledPane;
import javafx.scene.layout.VBox;

import javax.annotation.PostConstruct;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.ui.support.Style;

/**
 * Panel showing preferences/options that the user can set or modify.
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class PreferencePane extends VBox {

    public PreferencePane() {
        getStyleClass().addAll(Style.CONTAINER.css());
    }

    @PostConstruct
    void init() {
        getChildren().addAll(appearencePane(), behaviorPane(), workspacePane(), thumbnailsPane());
    }

    private TitledPane appearencePane() {
        return preferencePane(DefaultI18nContext.getInstance().i18n("Appearance"), new PreferenceAppearencePane());
    }

    private TitledPane behaviorPane() {
        return preferencePane(DefaultI18nContext.getInstance().i18n("Behavior"), new PreferenceBehaviorPane());
    }

    private TitledPane workspacePane() {
        return preferencePane(DefaultI18nContext.getInstance().i18n("Workspace"), new PreferenceWorkspacePane());
    }

    private TitledPane thumbnailsPane() {
        return preferencePane(DefaultI18nContext.getInstance().i18n("Thumbnails"), new PreferenceThumbnailsPane());
    }

    private TitledPane preferencePane(String titleString, Node node) {
        TitledPane pane = new TitledPane(titleString, node);
        pane.getStyleClass().addAll(Style.PREFERENCE_PANE.css());
        return pane;
    }
}
