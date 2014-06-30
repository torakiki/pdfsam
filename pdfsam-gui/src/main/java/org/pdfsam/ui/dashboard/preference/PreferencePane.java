/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;

/**
 * Panel showing preferences/options that the user can set or modify.
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class PreferencePane extends HBox {

    @Inject
    private PreferenceAppearencePane appearence;
    @Inject
    private PreferenceBehaviorPane behavior;
    @Inject
    private PreferenceWorkspacePane workspace;
    @Inject
    private PreferenceOutputPane output;
    @Inject
    private PreferenceThumbnailsPane thumbnails;

    public PreferencePane() {
        getStyleClass().add("dashboard-container");
    }

    @PostConstruct
    void init() {
        VBox left = new VBox(5);
        addSectionTitle(DefaultI18nContext.getInstance().i18n("Appearance"), left);
        left.getChildren().add(appearence);
        addSectionTitle(DefaultI18nContext.getInstance().i18n("Thumbnails"), left);
        left.getChildren().add(thumbnails);
        VBox right = new VBox(5);
        addSectionTitle(DefaultI18nContext.getInstance().i18n("Behavior"), right);
        right.getChildren().add(behavior);
        addSectionTitle(DefaultI18nContext.getInstance().i18n("Workspace"), right);
        right.getChildren().add(workspace);
        addSectionTitle(DefaultI18nContext.getInstance().i18n("Output"), right);
        right.getChildren().add(output);
        getChildren().addAll(left, right);
    }

    private void addSectionTitle(String title, Pane pane) {
        Label label = new Label(title);
        label.getStyleClass().add("section-title");
        pane.getChildren().add(label);
    }
}
