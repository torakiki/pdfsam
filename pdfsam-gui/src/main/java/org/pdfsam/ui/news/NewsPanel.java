/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23 ott 2015
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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
package org.pdfsam.ui.news;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.stream.Collectors;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.news.HideNewsPanelRequest;
import org.pdfsam.news.LatestNewsEvent;
import org.pdfsam.ui.support.Style;
import org.sejda.eventstudio.annotation.EventListener;

import de.jensd.fx.glyphs.GlyphsDude;
import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import javafx.application.Platform;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.ScrollPane.ScrollBarPolicy;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;

/**
 * VBox showing news
 * 
 * @author Andrea Vacondio
 *
 */
public class NewsPanel extends VBox {
    private VBox newsPanel = new VBox();
    ScrollPane scroll = new ScrollPane(newsPanel);

    public NewsPanel() {
        getStyleClass().add("news-panel");
        getStyleClass().addAll(Style.CONTAINER.css());
        Button closeButton = GlyphsDude.createIconButton(FontAwesomeIcon.TIMES);
        closeButton.getStyleClass().addAll("close-button");
        closeButton.setOnAction(e -> eventStudio().broadcast(HideNewsPanelRequest.INSTANCE));
        Label titleLabel = new Label(DefaultI18nContext.getInstance().i18n("What's new"));
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
    public void onLatestNews(LatestNewsEvent e) {
        Platform.runLater(() -> {
            newsPanel.getChildren().addAll(e.latestNews.stream().map(News::new).collect(Collectors.toList()));
        });
    }
}
