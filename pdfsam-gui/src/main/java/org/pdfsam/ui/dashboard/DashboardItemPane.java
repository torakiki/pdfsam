/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 04/set/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import static org.pdfsam.ui.commons.SetActiveModuleRequest.activeteCurrentModule;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import org.pdfsam.i18n.I18nContext;
import org.pdfsam.ui.commons.SetActiveModuleRequest;
import org.pdfsam.ui.support.Style;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.eventstudio.ReferenceStrength;

import javafx.scene.control.Button;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.ScrollPane.ScrollBarPolicy;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.text.TextAlignment;

/**
 * Pane showing a {@link DashboardItem} pane as center and having a footer with a close button whose purpose is to hide the dashboard and show the workarea.
 * 
 * @author Andrea Vacondio
 *
 */
class DashboardItemPane extends BorderPane {

    private DashboardItem item;
    private Listener<SetActiveModuleRequest> enableFooterListener = e -> {
        unregister();
        setBottom(buildFooter());
    };

    DashboardItemPane(DashboardItem item) {
        requireNotNullArg(item, "Dashboard item cannot be null");
        this.item = item;
        this.item.pane().getStyleClass().addAll(Style.DEAULT_CONTAINER.css());
        this.item.pane().getStyleClass().addAll(Style.CONTAINER.css());
        ScrollPane scroll = new ScrollPane(this.item.pane());
        scroll.getStyleClass().addAll(Style.DEAULT_CONTAINER.css());
        scroll.setFitToWidth(true);
        scroll.setHbarPolicy(ScrollBarPolicy.NEVER);
        scroll.setVbarPolicy(ScrollBarPolicy.AS_NEEDED);
        setCenter(scroll);
        eventStudio().add(SetActiveModuleRequest.class, enableFooterListener, Integer.MAX_VALUE,
                ReferenceStrength.STRONG);
    }

    private void unregister() {
        eventStudio().remove(SetActiveModuleRequest.class, enableFooterListener);
    }

    private HBox buildFooter() {
        Button closeButton = new Button(I18nContext.getInstance().i18n("Close"));
        closeButton.getStyleClass().addAll(Style.BUTTON.css());
        closeButton.setTextAlignment(TextAlignment.CENTER);
        closeButton.setOnAction(e -> eventStudio().broadcast(activeteCurrentModule()));
        HBox footer = new HBox(closeButton);
        footer.getStyleClass().addAll(Style.CLOSE_FOOTER.css());
        return footer;
    }

    DashboardItem getItem() {
        return item;
    }
}
