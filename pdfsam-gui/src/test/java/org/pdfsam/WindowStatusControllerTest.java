/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/ott/2014
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
package org.pdfsam;

import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.SetLatestStageStatusRequest;
import org.pdfsam.ui.StageStatus;
import org.pdfsam.ui.StageStatusService;
import org.sejda.eventstudio.Listener;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class WindowStatusControllerTest extends GuiTest {
    @Rule
    public ClearEventStudioRule eventStudioRule = new ClearEventStudioRule();

    private StageStatusService service;
    private WindowStatusController victim;
    private Stage victimStage;

    @Override
    protected Parent getRootNode() {
        victimStage = new Stage();
        VBox containerPane = new VBox();
        Scene scene = new Scene(containerPane);
        victimStage.setScene(scene);
        service = mock(StageStatusService.class);
        victim = new WindowStatusController(service);
        Button button = new Button("show");
        button.setOnAction(a -> victimStage.show());
        return button;
    }

    @Test
    @Ignore("It fails on CI server with xvfb")
    public void storeOnClose() throws Exception {
        when(service.getLatestStatus()).thenReturn(StageStatus.NULL);
        Listener<SetLatestStageStatusRequest> listener = mock(Listener.class);
        eventStudio().add(SetLatestStageStatusRequest.class, listener);
        FXTestUtils.invokeAndWait(() -> {
            victim.setStage(victimStage);
        }, 2);
        click("show").sleep(200);
        closeCurrentWindow();
        verify(listener).onEvent(any());
    }

    @Test
    public void defaultOnNullStatus() throws Exception {
        when(service.getLatestStatus()).thenReturn(StageStatus.NULL);
        FXTestUtils.invokeAndWait(() -> {
            victim.setStage(victimStage);
        }, 2);
        click("show").sleep(200);
        assertTrue(victimStage.isMaximized());
    }

    @Test
    public void defaultOnDisableRestore() throws Exception {
        when(service.getLatestStatus()).thenReturn(new StageStatus(10, 10, 10, 10));
        System.setProperty(WindowStatusController.PDFSAM_DISABLE_UI_RESTORE, Boolean.TRUE.toString());
        FXTestUtils.invokeAndWait(() -> {
            victim.setStage(victimStage);
        }, 2);
        click("show").sleep(200);
        assertTrue(victimStage.isMaximized());
        System.setProperty(WindowStatusController.PDFSAM_DISABLE_UI_RESTORE, Boolean.FALSE.toString());
    }
}
