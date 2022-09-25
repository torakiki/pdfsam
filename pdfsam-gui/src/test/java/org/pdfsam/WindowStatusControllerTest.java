/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/ott/2014
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
package org.pdfsam;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.service.ui.StageService;
import org.pdfsam.ui.StageStatus;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class WindowStatusControllerTest extends ApplicationTest {
    @Rule
    public ClearEventStudioRule eventStudioRule = new ClearEventStudioRule();

    private StageService service;
    private WindowStatusController victim;
    private Stage victimStage;

    @Override
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

    @Test
    public void defaultOnNullStatus() {
        when(service.getLatestStatus()).thenReturn(StageStatus.NULL);
        victim.setStage(victimStage);
        clickOn("show").sleep(200);
        assertTrue(victimStage.isMaximized());
    }

    @Test
    public void defaultOnDisableRestore() {
        when(service.getLatestStatus()).thenReturn(new StageStatus(10, 10, 10, 10));
        System.setProperty(WindowStatusController.PDFSAM_DISABLE_UI_RESTORE, Boolean.TRUE.toString());
        victim.setStage(victimStage);
        clickOn("show").sleep(200);
        assertTrue(victimStage.isMaximized());
        System.setProperty(WindowStatusController.PDFSAM_DISABLE_UI_RESTORE, Boolean.FALSE.toString());
    }
}
