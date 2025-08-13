/*
 * This file is part of the PDF Split And Merge source code
 * Created on 14/ott/2014
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
package org.pdfsam.ui.components.selection;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class })
@Tag("NoHeadless")
public class PasswordFieldPopupTest {
    @RegisterExtension
    static ClearEventStudioExtension staticExtension = new ClearEventStudioExtension("LogStage");
    private PdfDocumentDescriptor pdfDescriptor;

    @Start
    public void start(Stage stage) {
        pdfDescriptor = mock(PdfDocumentDescriptor.class);
        Button button = new Button("press");
        PasswordFieldPopup victim = new PasswordFieldPopup("module");
        victim.setId("victim");
        button.setOnAction(e -> victim.showFor(button, 0, 0, pdfDescriptor));
        Scene scene = new Scene(new HBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void pwdSentOnEnterKey(FxRobot robot) {
        robot.clickOn("press");
        Listener<PdfLoadRequest> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequest.class, listener);
        robot.write("myPwd").type(KeyCode.ENTER);
        verify(listener).onEvent(any());
        verify(pdfDescriptor).setPassword("myPwd");
    }

    @Test
    public void pwdSentOnButtonPressed(FxRobot robot) {
        robot.clickOn("press");
        Listener<PdfLoadRequest> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequest.class, listener);
        robot.write("myPwd").clickOn(".btn");
        verify(listener).onEvent(any());
        verify(pdfDescriptor).setPassword("myPwd");

    }
}
