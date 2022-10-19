/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13/lug/2014
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
package org.pdfsam.ui.components.commons;

import javafx.scene.Scene;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.mockito.ArgumentCaptor;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.model.io.NativeOpenUrlRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
public class UrlButtonTest {

    private static final String URL = "http://www.example.com";

    @Start
    private void start(Stage stage) {
        Scene scene = new Scene(new HBox(UrlButton.styledUrlButton("Chuck", URL, null)));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void nullUrl() {
        assertThrows(IllegalArgumentException.class, () -> UrlButton.urlButton("Chuck", null, UniconsLine.INFO_CIRCLE));
    }

    @Test
    public void emptyUrl() {
        assertThrows(IllegalArgumentException.class, () -> UrlButton.urlButton("Chuck", "", UniconsLine.INFO_CIRCLE));
    }

    @Test
    public void eventSent(FxRobot robot) {
        Listener<NativeOpenUrlRequest> listener = mock(Listener.class);
        eventStudio().add(NativeOpenUrlRequest.class, listener);
        robot.clickOn(".btn");
        ArgumentCaptor<NativeOpenUrlRequest> captor = ArgumentCaptor.forClass(NativeOpenUrlRequest.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(URL, captor.getValue().url());
    }

}
