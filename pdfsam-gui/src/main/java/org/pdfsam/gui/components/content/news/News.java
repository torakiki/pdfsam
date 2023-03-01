/*
 * This file is part of the PDF Split And Merge source code
 * Created on 23 ott 2015
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
package org.pdfsam.gui.components.content.news;

import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;
import javafx.scene.text.TextFlow;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.pdfsam.model.io.NativeOpenUrlRequest;
import org.pdfsam.model.news.NewsData;
import org.pdfsam.ui.components.commons.UrlButton;
import org.pdfsam.ui.components.support.Style;

import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * Panel showing one piece of news
 *
 * @author Andrea Vacondio
 */
public class News extends VBox {
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofLocalizedDate(FormatStyle.MEDIUM);

    News(NewsData data) {
        this.getStyleClass().add("news-box");
        TextFlow flow = new TextFlow();
        if (data.important()) {
            Text megaphone = FontIcon.of(UniconsLine.MEGAPHONE);
            megaphone.getStyleClass().clear();
            megaphone.getStyleClass().add("news-box-title-important");
            flow.getChildren().addAll(megaphone, new Text(" "));
        }

        Text titleText = new Text(data.title() + System.lineSeparator());
        titleText.setOnMouseClicked(e -> eventStudio().broadcast(new NativeOpenUrlRequest(data.link())));
        titleText.getStyleClass().add("news-box-title");

        Text contentText = new Text(data.content());
        contentText.setTextAlignment(TextAlignment.JUSTIFY);
        contentText.getStyleClass().add("news-content");

        flow.getChildren().addAll(titleText, contentText);
        flow.setTextAlignment(TextAlignment.JUSTIFY);
        Label labelDate = new Label(FORMATTER.format(data.date()), FontIcon.of(UniconsLine.CLOCK));
        labelDate.setPrefWidth(Integer.MAX_VALUE);
        HBox.setHgrow(labelDate, Priority.ALWAYS);
        HBox bottom = new HBox(labelDate);
        bottom.setAlignment(Pos.CENTER_LEFT);
        bottom.getStyleClass().add("news-box-footer");
        if (isNotBlank(data.link())) {
            Button link = UrlButton.urlButton(null, data.link(), UniconsLine.EXTERNAL_LINK_ALT,
                    Style.NEWS_BUTTON.css());
            bottom.getChildren().add(link);
        }
        getChildren().addAll(flow, bottom);
    }
}
