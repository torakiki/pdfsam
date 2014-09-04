/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/mag/2014
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
package org.pdfsam.ui.dashboard;

import static org.pdfsam.support.RequireUtils.requireNotNull;
import static org.pdfsam.ui.event.SetActiveModuleRequest.activeteCurrentModule;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javafx.animation.FadeTransition;
import javafx.scene.control.Button;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.scene.text.TextAlignment;
import javafx.util.Duration;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.ui.event.SetActiveDashboardItemRequest;
import org.pdfsam.ui.event.SetTitleEvent;
import org.pdfsam.ui.support.Style;
import org.sejda.eventstudio.annotation.EventListener;

/**
 * Panel showing the app dashboard
 * 
 * @author Andrea Vacondio
 */
@Named
public class Dashboard extends BorderPane {

    private Map<String, DashboardContentPane> items = new HashMap<>();
    private StackPane center = new StackPane();
    private FadeTransition fade = new FadeTransition(new Duration(300), center);

    @Inject
    public Dashboard(QuickbarDashboardPane navigation) {
        getStyleClass().addAll(Style.CONTAINER.css());
        setLeft(navigation);
        eventStudio().addAnnotatedListeners(this);
    }

    @Inject
    public Dashboard(List<DashboardItem> itemsList) {
        itemsList.stream().forEach(i -> items.put(i.id(), new DashboardContentPane(i)));
        fade.setFromValue(0);
        fade.setToValue(1);
        setCenter(center);
    }

    @EventListener
    public void onSetActiveDashboardItem(SetActiveDashboardItemRequest request) {
        DashboardContentPane requested = items.get(request.getActiveItemId());
        if (requested != null) {
            center.getChildren().setAll(requested);
            fade.play();
            eventStudio().broadcast(new SetTitleEvent(requested.item.name()));
        }
    }

    /**
     * Base class for a DashboardItem providing a footer to the item. A Close button is available in the footer and allow the user to hide the dashboard.
     * 
     * @author Andrea Vacondio
     *
     */
    private class DashboardContentPane extends BorderPane {

        private DashboardItem item;

        private DashboardContentPane(DashboardItem item) {
            requireNotNull(item, "Dashboard item cannot be null");
            this.item = item;
            this.item.pane().getStyleClass().addAll(Style.DEAULT_CONTAINER.css());
            this.item.pane().getStyleClass().addAll(Style.CONTAINER.css());
            setBottom(buildFooter());
            ScrollPane scroll = new ScrollPane(this.item.pane());
            scroll.setFitToHeight(true);
            scroll.setFitToWidth(true);
            setCenter(scroll);
        }

        private HBox buildFooter() {
            Button closeButton = new Button(DefaultI18nContext.getInstance().i18n("Close"));
            closeButton.getStyleClass().addAll(Style.BUTTON.css());
            closeButton.setTextAlignment(TextAlignment.CENTER);
            closeButton.setOnAction((e) -> eventStudio().broadcast(activeteCurrentModule()));
            HBox footer = new HBox(closeButton);
            footer.getStyleClass().addAll(Style.CLOSE_FOOTER.css());
            return footer;
        }

    }
}
