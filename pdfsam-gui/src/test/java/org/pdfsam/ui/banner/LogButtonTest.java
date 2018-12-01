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
package org.pdfsam.ui.banner;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.junit.Test;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.ui.commons.HideStageRequest;
import org.pdfsam.ui.commons.ShowStageRequest;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class LogButtonTest extends ApplicationTest {
    private LogButton victim;

    @Override
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
        clickOn(".button");
        assertTrue(listener.isHit());
        assertFalse(hideListener.isHit());
        clickOn(".button");
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
