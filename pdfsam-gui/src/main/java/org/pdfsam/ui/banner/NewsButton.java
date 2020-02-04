/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23 ott 2015
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
package org.pdfsam.ui.banner;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.news.HideNewsPanelRequest;
import org.pdfsam.news.ShowNewsPanelRequest;
import org.pdfsam.ui.commons.Animations;
import org.pdfsam.eventstudio.annotation.EventListener;

import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import javafx.animation.Timeline;
import javafx.scene.control.Tooltip;

/**
 * Button requesting to open the news panel
 * 
 * @author Andrea Vacondio
 */
class NewsButton extends BannerButton {

    static final String UP_TO_DATE_CSS_CLASS = "news-not-up-to-date";
    private Timeline anim;
    private Object action = ShowNewsPanelRequest.INSTANCE;

    NewsButton() {
        super(MaterialDesignIcon.NEWSPAPER);
        setOnAction(e -> {
            eventStudio().broadcast(action);
            action = switchAction();
        });
        setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("What's new")));
        anim = Animations.shake(this);
        eventStudio().addAnnotatedListeners(this);
    }

    private Object switchAction() {
        if (action instanceof ShowNewsPanelRequest) {
            return HideNewsPanelRequest.INSTANCE;
        }
        return ShowNewsPanelRequest.INSTANCE;
    }
    /**
     * Sets the button in upToDate state or else, based on the input value
     * 
     * @param value
     */
    public void setUpToDate(boolean value) {
        if (value) {
            getStyleClass().remove(UP_TO_DATE_CSS_CLASS);
            anim.stop();
            setRotate(0);
            setScaleY(1);
        } else {
            getStyleClass().add(UP_TO_DATE_CSS_CLASS);
            anim.play();
        }
    }

    @EventListener
    public void onShowNewsPanel(ShowNewsPanelRequest req) {
        setUpToDate(true);
    }
}
