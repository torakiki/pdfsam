/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 07/nov/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.banner;

import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.animation.Animation.Status;
import javafx.animation.FadeTransition;
import javafx.scene.control.Label;
import javafx.util.Duration;

import javax.annotation.PostConstruct;
import javax.inject.Named;

import org.pdfsam.gui.log.ChangedVisiblityLogAreaEvent;
import org.pdfsam.gui.log.ErrorLoggedEvent;
import org.sejda.eventstudio.annotation.EventListener;

/**
 * Display a little notification with the number of unread error messages
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class ErrorsNotification extends Label {
    private int unreadMessages = 0;
    private FadeTransition fade;

    public ErrorsNotification() {
        getStyleClass().add("logs-notification");
        setVisible(false);
        setText("!");
        eventStudio().addAnnotatedListeners(this);
    }

    @PostConstruct
    private void init() {
        fade = new FadeTransition(Duration.millis(200), this);
        fade.setAutoReverse(true);
    }

    @EventListener
    public void onLogMessage(ErrorLoggedEvent event) {
        unreadMessages++;
        if (!isVisible() && !(fade.getStatus() == Status.RUNNING)) {
            setVisible(true);
            fade.setFromValue(0);
            fade.setToValue(1);
            fade.setOnFinished(null);
            fade.play();
        }
    }

    @EventListener
    public void onViewedLogArea(ChangedVisiblityLogAreaEvent event) {
        if (hasUnreadMessages() && !(fade.getStatus() == Status.RUNNING)) {
            fade.setFromValue(1);
            fade.setToValue(0);
            fade.setOnFinished(e -> {
                setVisible(false);
                unreadMessages = 0;
            });
            fade.play();
        }
    }

    public boolean hasUnreadMessages() {
        return unreadMessages > 0;
    }

}
