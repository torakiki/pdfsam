/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20/ott/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import java.util.Collection;
import java.util.function.Consumer;

import javafx.concurrent.Worker.State;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.layout.BorderPane;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import javafx.stage.Stage;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.ui.commons.ClosePane;
import org.pdfsam.ui.commons.HideOnEscapeHandler;
import org.pdfsam.ui.commons.OpenUrlRequest;
import org.pdfsam.ui.support.Style;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.w3c.dom.NodeList;
import org.w3c.dom.events.EventTarget;
import org.w3c.dom.html.HTMLAnchorElement;

/**
 * Stage showing the latest news
 * 
 * @author Andrea Vacondio
 *
 */
@Lazy
@Named
public class NewsStage extends Stage {
    private static final Logger LOG = LoggerFactory.getLogger(NewsStage.class);

    private WebView browser = new WebView();
    private Consumer<Boolean> onLoaded = showing -> {
        LOG.trace("What's new page loaded");
        if (!showing) {
            centerOnScreen();
            show();
        }
        requestFocus();
    };

    @Inject
    public NewsStage(Collection<Image> logos, StylesConfig styles) {
        BorderPane containerPane = new BorderPane();
        containerPane.getStyleClass().addAll(Style.CONTAINER.css());
        containerPane.getStyleClass().add("-pdfsam-news-pane");
        containerPane.setCenter(browser);
        containerPane.setBottom(new ClosePane());
        Scene scene = new Scene(containerPane);
        scene.getStylesheets().addAll(styles.styles());
        scene.setOnKeyReleased(new HideOnEscapeHandler(this));
        setScene(scene);
        setTitle(DefaultI18nContext.getInstance().i18n("What's new"));
        getIcons().addAll(logos);
        setMaximized(false);
    }

    void loadAndShow(Consumer<Boolean> onSuccess) {
        WebEngine webEngine = browser.getEngine();
        webEngine.getLoadWorker().stateProperty().addListener((o, oldVal, newVal) -> {
            if (newVal == State.SUCCEEDED) {
                wrapHrefToOpenNative(webEngine);
                onLoaded.andThen(onSuccess).accept(isShowing());
            }
        });
        webEngine.load("http://www.pdfsam.org/latest");
    }

    private void wrapHrefToOpenNative(WebEngine webEngine) {
        // TODO find a better way
        NodeList nodeList = webEngine.getDocument().getElementsByTagName("a");
        for (int i = 0; i < nodeList.getLength(); i++) {
            EventTarget eventTarget = (EventTarget) nodeList.item(i);
            eventTarget.addEventListener("click", evt -> {
                HTMLAnchorElement anchorElement = (HTMLAnchorElement) evt.getCurrentTarget();
                eventStudio().broadcast(new OpenUrlRequest(anchorElement.getHref()));
                evt.preventDefault();
            }, false);
        }
    }
}
