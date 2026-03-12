/*
 * This file is part of the PDF Split And Merge source code
 * Created on 05/mag/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.gui.components.content.home;

import jakarta.inject.Inject;
import javafx.application.Platform;
import javafx.scene.control.Label;
import javafx.scene.layout.FlowPane;
import javafx.scene.layout.VBox;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.model.premium.PremiumTool;
import org.pdfsam.model.premium.PremiumToolsResponse;
import org.pdfsam.model.tool.Tool;

import java.util.Collection;
import java.util.Comparator;
import java.util.Map;

import static java.util.stream.Collectors.groupingBy;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Panel showing tools tiles that the user can click to select the tool he/she wants to use.
 *
 * @author Andrea Vacondio
 */
public class HomePane extends VBox {

    @Inject
    public HomePane() {
        this(app().runtimeState().tools().values());
    }

    public HomePane(Collection<Tool> tools) {
        getStyleClass().addAll("spaced-container");
        tools.stream().collect(groupingBy(t -> t.descriptor().category())).entrySet().stream()
                .sorted(Map.Entry.comparingByKey()).forEach(entry -> {
                    var toolCategory = new Label(entry.getKey().getDescription());
                    toolCategory.getStyleClass().add("modules-tile-title");
                    this.getChildren().add(toolCategory);
                    var toolsPane = new FlowPane();
                    toolCategory.setLabelFor(toolsPane);
                    toolsPane.getStyleClass().add("home-tools");
                    entry.getValue().stream().sorted(Comparator.comparing(t -> t.descriptor().name())).map(ToolsHomeTile::new)
                            .forEach(toolsPane.getChildren()::add);
                    this.getChildren().add(toolsPane);
                });
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onPremiumModules(PremiumToolsResponse e) {
        if (!e.premiumTools().isEmpty()) {
            Label premiumTile = new Label(i18n().tr("Premium features"));
            premiumTile.getStyleClass().add("modules-tile-title");
            var premiumToolsPanel = new FlowPane();
            premiumTile.setLabelFor(premiumToolsPanel);
            premiumToolsPanel.getStyleClass().add("home-tools");
            e.premiumTools().stream().sorted(Comparator.comparingInt(PremiumTool::id)).map(PremiumToolTile::new)
                    .forEach(premiumToolsPanel.getChildren()::add);
            Platform.runLater(() -> this.getChildren().addAll(premiumTile, premiumToolsPanel));
        }
    }

    @Override
    protected double computeMinWidth(double height) {
        return 0.0;
    }
}
