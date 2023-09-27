/*
 * This file is part of the PDF Split And Merge source code
 * Created on 08/ott/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.gui;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.core.ConfigurableSystemProperty;
import org.pdfsam.model.ui.StageStatus;
import org.pdfsam.service.ui.StageService;
import org.pdfsam.test.ClearEventStudioExtension;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.framework.junit5.Stop;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * @author Andrea Vacondio
 *
 */
@ExtendWith({ApplicationExtension.class, ClearEventStudioExtension.class})
public class WindowStatusControllerTest   {

    private StageService service;
    private WindowStatusController victim;
    private Stage victimStage;
    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        victimStage = new Stage();
        victimStage.setScene(new Scene(new VBox()));

        service = mock(StageService.class);
        victim = new WindowStatusController(service);
        Button button = new Button("show");
        button.setOnAction(a -> victimStage.show());
        Scene scene = new Scene(new HBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Stop
    public void stop(){
        victimStage.close();
    }

    @Test
    public void defaultOnNullStatus() {
        when(service.getLatestStatus()).thenReturn(StageStatus.NULL);
        victim.setStage(victimStage);
        robot.clickOn("show").sleep(200);
        assertTrue(victimStage.isMaximized());
    }

    @Test
    public void defaultOnDisableRestore() {
        when(service.getLatestStatus()).thenReturn(new StageStatus(10, 10, 10, 10));
        System.setProperty(ConfigurableSystemProperty.PDFSAM_DISABLE_UI_RESTORE, Boolean.TRUE.toString());
        victim.setStage(victimStage);
        robot.clickOn("show").sleep(200);
        assertTrue(victimStage.isMaximized());
        System.setProperty(ConfigurableSystemProperty.PDFSAM_DISABLE_UI_RESTORE, Boolean.FALSE.toString());
    }
}
