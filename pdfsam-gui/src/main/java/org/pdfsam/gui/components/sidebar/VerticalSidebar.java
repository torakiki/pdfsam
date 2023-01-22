package org.pdfsam.gui.components.sidebar;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 09/01/23
 * Copyright 2023 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import jakarta.inject.Inject;
import javafx.geometry.Orientation;
import javafx.scene.control.Label;
import javafx.scene.control.Separator;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.gui.components.content.about.AboutContentItem;
import org.pdfsam.gui.components.content.home.HomeContentItem;
import org.pdfsam.gui.components.content.log.LogContentItem;
import org.pdfsam.gui.components.content.preference.PreferenceContentItem;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.ui.SetActiveContentItemRequest;
import org.pdfsam.model.ui.ShowErrorMessagesRequest;

import java.util.Comparator;
import java.util.Map;

import static javafx.scene.layout.VBox.setVgrow;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.gui.components.sidebar.SelectableSidebarButton.of;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * A vertical sidebar that can be opened and closed
 *
 * @author Andrea Vacondio
 */
public class VerticalSidebar extends BorderPane {

    private VBox buttons = new VBox();

    @Inject
    public VerticalSidebar(HomeContentItem homeItem, LogButton logButton, NewsButton newsButton,
            PreferenceContentItem preferenceItem, AboutContentItem aboutItem, WorkspaceButton workspaceButton) {
        this(homeItem, logButton, newsButton, preferenceItem, aboutItem, workspaceButton, app().runtimeState().tools());
    }

    public VerticalSidebar(HomeContentItem homeItem, LogButton logButton, NewsButton newsButton,
            PreferenceContentItem preferenceItem, AboutContentItem aboutItem, WorkspaceButton workspaceButton,
            Map<String, Tool> tools) {
        getStyleClass().add("vertical-sidebar");
        var expandButton = new ExpandButton();
        this.setTop(expandButton);
        buttons.getStyleClass().add("sidebar-buttons");

        addButton(of(homeItem), expandButton);
        var toolsLabel = new Label(i18n().tr("Tools").toUpperCase());
        toolsLabel.getStyleClass().add("sidebar-title");
        buttons.getChildren().addAll(new Separator(Orientation.HORIZONTAL), toolsLabel);
        Comparator<Tool> comparator = Comparator.comparing(t -> t.descriptor().category());
        tools.values().stream().sorted(comparator.thenComparing(m -> m.descriptor().name()))
                .map(SelectableSidebarButton::of).forEach(b -> {
                    addButton(b, expandButton);
                });
        var spacer = new Region();
        spacer.getStyleClass().add("spacer");
        setVgrow(spacer, Priority.ALWAYS);
        buttons.getChildren().add(spacer);
        addButton(workspaceButton, expandButton);
        addButton(logButton, expandButton);
        addButton(newsButton, expandButton);
        addButton(of(aboutItem), expandButton);
        addButton(of(preferenceItem), expandButton);
        buttons.getChildren().add(new Separator(Orientation.HORIZONTAL));
        addButton(new ExitButton(), expandButton);
        setCenter(buttons);
        eventStudio().addAnnotatedListeners(this);
    }

    private void addButton(SidebarButton button, ExpandButton expandButton) {
        button.displayTextProperty().bind(expandButton.selectedProperty());
        buttons.getChildren().add(button);
    }

    private void addButton(SidebarButtonWithNotification button, ExpandButton expandButton) {
        button.displayTextProperty().bind(expandButton.selectedProperty());
        buttons.getChildren().add(button);
    }

    @EventListener
    public void onSetCurrentContentItem(SetActiveContentItemRequest r) {
        for (var child : buttons.getChildren()) {
            if (child instanceof Selectable button) {
                button.selectIf(r.id());
            }
        }
    }

    @EventListener
    public void onShowErrorMessagesRequest(ShowErrorMessagesRequest request) {
        onSetCurrentContentItem(new SetActiveContentItemRequest(LogContentItem.ID));
    }
}
