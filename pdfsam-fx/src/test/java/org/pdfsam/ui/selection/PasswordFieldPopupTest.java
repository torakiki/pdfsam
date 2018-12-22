/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/ott/2014
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
package org.pdfsam.ui.selection;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.pdfsam.NoHeadless;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.eventstudio.Listener;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class PasswordFieldPopupTest extends ApplicationTest {
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule("LogStage");
    private PdfDocumentDescriptor pdfDescriptor;

    @Override
    public void start(Stage stage) {
        pdfDescriptor = mock(PdfDocumentDescriptor.class);
        Button button = new Button("press");
        PasswordFieldPopup victim = new PasswordFieldPopup("module");
        victim.setId("victim");
        button.setOnAction(e -> victim.showFor(button, pdfDescriptor, 0, 0));
        Scene scene = new Scene(new HBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void pwdSentOnEnterKey() {
        clickOn("press");
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        write("myPwd").type(KeyCode.ENTER);
        verify(listener).onEvent(any());
        verify(pdfDescriptor).setPassword("myPwd");
    }

    @Test
    @Category(NoHeadless.class)
    public void pwdSentOnButtonPressed() {
        clickOn("press");
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        write("myPwd").clickOn(".pdfsam-button");
        verify(listener).onEvent(any());
        verify(pdfDescriptor).setPassword("myPwd");

    }
}
