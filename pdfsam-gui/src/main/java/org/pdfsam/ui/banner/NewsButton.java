/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23 ott 2015
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
package org.pdfsam.ui.banner;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import javax.inject.Named;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.news.ShowNewsPanelRequest;
import org.pdfsam.ui.commons.Animations;
import org.sejda.eventstudio.annotation.EventListener;

import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import javafx.animation.Timeline;
import javafx.scene.control.Tooltip;

/**
 * Button requesting to open the news panel
 * 
 * @author Andrea Vacondio
 */
@Named
class NewsButton extends BannerButton {

    static final String UP_TO_DATE_CSS_CLASS = "news-not-up-to-date";
    private Timeline anim;

    NewsButton() {
        super(MaterialDesignIcon.NEWSPAPER);
        setOnAction(e -> eventStudio().broadcast(ShowNewsPanelRequest.INSTANCE));
        setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("What's new")));
        anim = Animations.shake(this);
        eventStudio().addAnnotatedListeners(this);
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
