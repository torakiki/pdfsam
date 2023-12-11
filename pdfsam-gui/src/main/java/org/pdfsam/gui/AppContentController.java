package org.pdfsam.gui;
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

import jakarta.inject.Inject;
import javafx.animation.FadeTransition;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Node;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;
import javafx.util.Duration;
import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.gui.components.content.log.LogContentItem;
import org.pdfsam.gui.components.content.news.NewsPanel;
import org.pdfsam.injector.Auto;
import org.pdfsam.model.news.HideNewsPanelRequest;
import org.pdfsam.model.news.ToggleNewsPanelRequest;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.ui.ContentItem;
import org.pdfsam.model.ui.SetActiveContentItemRequest;
import org.pdfsam.model.ui.SetTitleRequest;
import org.pdfsam.model.ui.ShowLogMessagesRequest;
import org.tinylog.Logger;
import org.tinylog.Supplier;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * Controller for the application content
 *
 * @author Andrea Vacondio
 */
@Auto
public class AppContentController {

    private final AppContainer container;
    private final Map<String, Item> items = new HashMap<>();
    private final VBox news;
    private final ApplicationContext context;

    @Inject
    public AppContentController(AppContainer container, NewsPanel news, List<ContentItem> itemsList) {
        this(container, news, itemsList, app());
    }

    AppContentController(AppContainer container, NewsPanel news, List<ContentItem> itemsList,
            ApplicationContext context) {
        this.container = container;
        this.context = context;
        this.news = new VBox(news);
        this.news.getStyleClass().add("news-panel-container");
        itemsList.stream().filter(i -> !i.disabled())
                .forEach(i -> items.put(i.id(), new Item(i.name(), i::panel, null)));
        context.runtimeState().tools().values()
                .forEach(t -> items.put(t.id(), new Item(t.descriptor().name(), t::panel, t)));
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onSetActiveContentItem(SetActiveContentItemRequest request) {
        Logger.trace("Set active content request to '{}'", request.id());
        var item = items.get(request.id());
        if (nonNull(item)) {
            setActivePane(item.paneSupplier().get());
            eventStudio().broadcast(new SetTitleRequest(item.name()));
        }
        context.runtimeState().activeTool(ofNullable(item).map(Item::tool).orElse(null));
    }

    @EventListener
    public void onShowLogMessagesRequest(ShowLogMessagesRequest request) {
        //when log viewer is shown we don't want to clear the active tool so that the Close button in the log viewer will show and return to the active tool
        Logger.trace("Set active content request to Log content");
        var item = items.get(LogContentItem.ID);
        if (nonNull(item)) {
            setActivePane(item.paneSupplier().get());
            eventStudio().broadcast(new SetTitleRequest(item.name()));
        }
    }

    @EventListener(priority = Integer.MIN_VALUE)
    public void onShowNewsPanel(ToggleNewsPanelRequest request) {
        ofNullable(this.container.getRight()).ifPresentOrElse(
                (d) -> fadeOut(news, (a) -> this.container.setRight(null)), this::fadeInNews);
    }

    @EventListener(priority = Integer.MIN_VALUE)
    public void onHideNewsPanel(HideNewsPanelRequest request) {
        fadeOut(news, (a) -> this.container.setRight(null));
    }

    private void setActivePane(Pane panel) {
        if (nonNull(panel) && !panel.equals(this.container.getContent())) {
            fadeOut(this.container.getContent(), null);
            fadeIn(panel);
        }
    }

    private void fadeOut(Node node, EventHandler<ActionEvent> onFinish) {
        if (nonNull(node)) {
            var fade = new FadeTransition(new Duration(150), node);
            fade.setFromValue(1);
            fade.setToValue(0);
            fade.play();
            if (nonNull(onFinish)) {
                fade.setOnFinished(onFinish);
            }
        }
    }

    private void fadeIn(Pane newNode) {
        var fade = new FadeTransition(new Duration(150), newNode);
        fade.setFromValue(0);
        fade.setToValue(1);
        newNode.setOpacity(0);
        this.container.setContent(newNode);
        fade.play();
    }

    private void fadeInNews() {
        var fade = new FadeTransition(new Duration(150), news);
        fade.setFromValue(0);
        fade.setToValue(1);
        news.setOpacity(0);
        this.container.setRight(news);
        fade.play();
    }

    private record Item(String name, Supplier<Pane> paneSupplier, Tool tool) {
    }

}
