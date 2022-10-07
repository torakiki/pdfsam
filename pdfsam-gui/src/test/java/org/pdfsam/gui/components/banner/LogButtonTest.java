/*
 * This file is part of the PDF Split And Merge source code
 * Created on 20/ago/2014
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
package org.pdfsam.gui.components.banner;

import javafx.scene.Scene;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.model.ui.HideStageRequest;
import org.pdfsam.model.ui.ShowStageRequest;
import org.pdfsam.test.HitTestListener;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class })
public class LogButtonTest {
    private LogButton victim;
    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        victim = new LogButton();
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void onClick() {
        HitTestListener<ShowStageRequest> listener = new HitTestListener<>();
        HitTestListener<HideStageRequest> hideListener = new HitTestListener<>();
        eventStudio().add(ShowStageRequest.class, listener, "LogStage");
        eventStudio().add(HideStageRequest.class, hideListener, "LogStage");
        robot.clickOn(".button");
        assertTrue(listener.isHit());
        assertFalse(hideListener.isHit());
        robot.clickOn(".button");
        assertTrue(hideListener.isHit());
    }

    @Test
    public void setUpToDate() {
        assertFalse(victim.getStyleClass().contains(LogButton.HAS_ERRORS_CSS_CLASS));
        victim.hasUnseenErrors(true);
        assertTrue(victim.getStyleClass().contains(LogButton.HAS_ERRORS_CSS_CLASS));
        victim.hasUnseenErrors(false);
        assertFalse(victim.getStyleClass().contains(LogButton.HAS_ERRORS_CSS_CLASS));
    }
}
