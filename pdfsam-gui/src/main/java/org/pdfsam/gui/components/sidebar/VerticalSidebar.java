/*
 * This file is part of the PDF Split And Merge source code
 * Created on 09/01/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.gui.components.sidebar;

import jakarta.inject.Inject;
import javafx.geometry.Orientation;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
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
import org.pdfsam.model.ui.ContentItem;
import org.pdfsam.model.ui.SetActiveContentItemRequest;
import org.pdfsam.model.ui.ShowLogMessagesRequest;

import static javafx.scene.layout.VBox.setVgrow;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.gui.components.sidebar.SelectableSidebarButton.of;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * A vertical sidebar that can be opened and closed
 *
 * @author Andrea Vacondio
 */
public class VerticalSidebar extends BorderPane {

    private final ToolsButtons tools;
    private final VBox buttons = new VBox();

    @Inject
    public VerticalSidebar(HomeContentItem homeItem, LogButton logButton, NewsButton newsButton,
            PreferenceContentItem preferenceItem, AboutContentItem aboutItem, WorkspaceButton workspaceButton,
            ToolsButtons tools) {
        getStyleClass().add("vertical-sidebar");
        this.tools = tools;
        var expandButton = new ExpandButton();
        this.setTop(expandButton);
        buttons.getStyleClass().add("sidebar-buttons");

        addButton(homeItem, expandButton);
        var toolsLabel = new Label(i18n().tr("Tools").toUpperCase());
        toolsLabel.getStyleClass().add("sidebar-title");
        toolsLabel.contentDisplayProperty()
                .bind(expandButton.selectedProperty().map(b -> b ? ContentDisplay.LEFT : ContentDisplay.GRAPHIC_ONLY));
        toolsLabel.setLabelFor(buttons);
        buttons.getChildren().addAll(new Separator(Orientation.HORIZONTAL), toolsLabel);
        tools.getChildren().forEach(b -> {
            if (b instanceof SidebarButton button) {
                button.displayTextProperty().bind(expandButton.selectedProperty());
            }
        });
        buttons.getChildren().add(tools);
        var spacer = new Region();
        spacer.getStyleClass().add("spacer");
        setVgrow(spacer, Priority.ALWAYS);
        buttons.getChildren().add(spacer);
        addButton(workspaceButton, expandButton);
        addButton(logButton, expandButton);
        addButton(newsButton, expandButton);
        addButton(aboutItem, expandButton);
        addButton(preferenceItem, expandButton);
        buttons.getChildren().add(new Separator(Orientation.HORIZONTAL));
        addButton(new ExitButton(), expandButton);
        var scroll = new ScrollPane(buttons);
        scroll.minViewportWidthProperty().bind(buttons.layoutBoundsProperty().map(_ -> buttons.minWidth(-1)));
        scroll.getStyleClass().add("sidebar-scroll");
        setCenter(scroll);
        eventStudio().addAnnotatedListeners(this);
    }

    private void addButton(ContentItem item, ExpandButton expandButton) {
        if (!item.disabled()) {
            var button = of(item);
            button.displayTextProperty().bind(expandButton.selectedProperty());
            buttons.getChildren().add(button);
        }
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
        for (var child : tools.getChildren()) {
            if (child instanceof Selectable button) {
                button.selectIf(r.id());
            }
        }

    }

    @EventListener
    public void onShowErrorMessagesRequest(ShowLogMessagesRequest request) {
        onSetCurrentContentItem(new SetActiveContentItemRequest(LogContentItem.ID));
    }
}
