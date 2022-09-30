package org.pdfsam.ui.components.commons;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 30/09/22
 * Copyright 2022 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import javafx.scene.Scene;
import javafx.stage.Stage;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.model.ui.HideStageRequest;
import org.pdfsam.test.HitTestListener;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(ApplicationExtension.class)
public class ClosePaneCustomTest {

    @Start
    private void start(Stage stage) {
        Scene scene = new Scene(new ClosePane(k -> eventStudio().broadcast(HideStageRequest.INSTANCE)));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    @Tag("NoHeadless")
    public void customAction(FxRobot robot) {
        HitTestListener<HideStageRequest> listener = new HitTestListener<>();
        eventStudio().add(HideStageRequest.class, listener);
        robot.clickOn(".pdfsam-button");
        assertTrue(listener.isHit());
    }
}
