/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23 ott 2015
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;

import org.pdfsam.news.NewsData;
import org.pdfsam.ui.commons.OpenUrlRequest;
import org.pdfsam.ui.commons.UrlButton;

import de.jensd.fx.glyphs.GlyphsDude;
import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;
import javafx.scene.text.TextFlow;

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
        if (data.isImportant()) {
            Text megaphone = GlyphsDude.createIcon(FontAwesomeIcon.BULLHORN, "1.2em");
            megaphone.getStyleClass().clear();
            megaphone.getStyleClass().add("news-box-title-important");
            flow.getChildren().addAll(megaphone, new Text(" "));
        }

        Text titleText = new Text(data.getTitle() + System.lineSeparator());
        titleText.setOnMouseClicked(e -> eventStudio().broadcast(new OpenUrlRequest(data.getLink())));
        titleText.getStyleClass().add("news-box-title");

        Text contentText = new Text(data.getContent());
        contentText.setTextAlignment(TextAlignment.JUSTIFY);
        contentText.getStyleClass().add("news-content");

        flow.getChildren().addAll(titleText, contentText);
        flow.setTextAlignment(TextAlignment.JUSTIFY);
        Label labelDate = new Label(FORMATTER.format(data.getDate()), GlyphsDude.createIcon(MaterialDesignIcon.CLOCK));
        labelDate.setPrefWidth(Integer.MAX_VALUE);
        HBox.setHgrow(labelDate, Priority.ALWAYS);
        HBox bottom = new HBox(labelDate);
        bottom.setAlignment(Pos.CENTER_LEFT);
        bottom.getStyleClass().add("news-box-footer");
        if (isNotBlank(data.getLink())) {
            Button link = UrlButton.urlButton(null, data.getLink(), FontAwesomeIcon.EXTERNAL_LINK_SQUARE,
                    "pdfsam-toolbar-button");
            bottom.getChildren().add(link);
        }
        getChildren().addAll(flow, bottom);
    }
}
