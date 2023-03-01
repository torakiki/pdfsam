/*
 * This file is part of the PDF Split And Merge source code
 * Created on 28/ott/2013
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
package org.pdfsam.gui.components.content.preference;

import jakarta.inject.Inject;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import org.pdfsam.ui.components.support.Style;

import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Panel showing preferences/options that the user can set or modify.
 * 
 * @author Andrea Vacondio
 * 
 */
public class PreferencePane extends HBox {

    @Inject
    public PreferencePane(PreferenceAppearencePane appearance, PreferenceBehaviorPane behavior,
            PreferenceWorkspacePane workspace, PreferenceOutputPane output) {
        getStyleClass().add("spaced-container");
        VBox left = new VBox(Style.DEFAULT_SPACING);
        left.setMinWidth(USE_PREF_SIZE);
        addSectionTitle(i18n().tr("Appearance"), left);
        left.getChildren().add(appearance);
        addSectionTitle(i18n().tr("Behavior"), left);
        left.getChildren().add(behavior);
        VBox right = new VBox(Style.DEFAULT_SPACING);
        HBox.setHgrow(right, Priority.ALWAYS);
        addSectionTitle(i18n().tr("Workspace"), right);
        right.getChildren().add(workspace);
        addSectionTitle(i18n().tr("Output"), right);
        right.getChildren().add(output);
        getChildren().addAll(left, right);
    }

    private void addSectionTitle(String title, Pane pane) {
        Label label = new Label(title);
        label.getStyleClass().add("section-title");
        pane.getChildren().add(label);
    }
}
