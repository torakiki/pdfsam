/*
 * This file is part of the PDF Split And Merge source code
 * Created on 23 ott 2015
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
package org.pdfsam.gui.components.content.news;

import javafx.application.Platform;
import javafx.geometry.Pos;
import javafx.scene.AccessibleRole;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.ScrollPane.ScrollBarPolicy;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.model.news.HideNewsPanelRequest;
import org.pdfsam.model.news.LatestNewsResponse;
import org.pdfsam.ui.components.support.Style;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * VBox showing news
 *
 * @author Andrea Vacondio
 */
public class NewsPanel extends VBox {
    private final VBox newsPanel = new VBox();
    final ScrollPane scroll = new ScrollPane(newsPanel);

    public NewsPanel() {
        getStyleClass().add("news-panel");
        getStyleClass().addAll(Style.CONTAINER.css());
        newsPanel.getStyleClass().add("news-panel-content");
        newsPanel.setAccessibleRole(AccessibleRole.PARENT);
        newsPanel.setAccessibleText(i18n().tr("News items"));
        var closeIcon = FontIcon.of(UniconsLine.TIMES);
        closeIcon.setAccessibleRole(AccessibleRole.IMAGE_VIEW);
        var closeButton = new Button("", closeIcon);
        closeButton.getStyleClass().addAll("close-button");
        closeButton.setAccessibleText(i18n().tr("Close news panel"));
        closeButton.setOnAction(e -> eventStudio().broadcast(HideNewsPanelRequest.INSTANCE));
        Label titleLabel = new Label(i18n().tr("What's new"));
        titleLabel.setPrefWidth(Integer.MAX_VALUE);
        titleLabel.getStyleClass().add("news-panel-title");

        StackPane top = new StackPane(titleLabel, closeButton);
        top.setAlignment(Pos.TOP_RIGHT);

        scroll.getStyleClass().add("scrollable-news");
        scroll.setHbarPolicy(ScrollBarPolicy.NEVER);
        scroll.setFitToHeight(true);
        scroll.setFitToWidth(true);
        getChildren().addAll(top, scroll);

        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onLatestNews(LatestNewsResponse e) {
        Platform.runLater(() -> newsPanel.getChildren().addAll(e.latestNews().stream().map(News::new).toList()));
    }
}
