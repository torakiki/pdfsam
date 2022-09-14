/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/ott/2013
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
package org.pdfsam.ui.dashboard.preference;

import javax.inject.Inject;

import org.pdfsam.i18n.I18nContext;
import org.pdfsam.ui.support.Style;

import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;

/**
 * Panel showing preferences/options that the user can set or modify.
 * 
 * @author Andrea Vacondio
 * 
 */
public class PreferencePane extends HBox {

    @Inject
    public PreferencePane(PreferenceAppearencePane appearence, PreferenceBehaviorPane behavior,
            PreferenceWorkspacePane workspace, PreferenceOutputPane output) {
        getStyleClass().add("dashboard-container");
        VBox left = new VBox(Style.DEFAULT_SPACING);
        left.setMinWidth(USE_PREF_SIZE);
        addSectionTitle(I18nContext.getInstance().i18n("Appearance"), left);
        left.getChildren().add(appearence);
        addSectionTitle(I18nContext.getInstance().i18n("Behavior"), left);
        left.getChildren().add(behavior);
        VBox right = new VBox(Style.DEFAULT_SPACING);
        HBox.setHgrow(right, Priority.ALWAYS);
        addSectionTitle(I18nContext.getInstance().i18n("Workspace"), right);
        right.getChildren().add(workspace);
        addSectionTitle(I18nContext.getInstance().i18n("Output"), right);
        right.getChildren().add(output);
        getChildren().addAll(left, right);
    }

    private void addSectionTitle(String title, Pane pane) {
        Label label = new Label(title);
        label.getStyleClass().add("section-title");
        pane.getChildren().add(label);
    }
}
