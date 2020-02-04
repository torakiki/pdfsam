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
package org.pdfsam.ui.commons;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.eventstudio.Listener;
import org.testfx.framework.junit.ApplicationTest;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import javafx.scene.Scene;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class UrlButtonTest extends ApplicationTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    private static final String URL = "http://www.example.com";

    @Override
    public void start(Stage stage) {
        Scene scene = new Scene(new HBox(UrlButton.styledUrlButton("Chuck", URL, null)));
        stage.setScene(scene);
        stage.show();
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullUrl() {
        UrlButton.urlButton("Chuck", null, FontAwesomeIcon.ADN);
    }

    @Test(expected = IllegalArgumentException.class)
    public void emptyUrl() {
        UrlButton.urlButton("Chuck", "", FontAwesomeIcon.ADN);
    }

    @Test
    public void eventSent() {
        Listener<OpenUrlRequest> listener = mock(Listener.class);
        eventStudio().add(OpenUrlRequest.class, listener);
        clickOn(".pdfsam-button");
        ArgumentCaptor<OpenUrlRequest> captor = ArgumentCaptor.forClass(OpenUrlRequest.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(URL, captor.getValue().getUrl());
    }

}
