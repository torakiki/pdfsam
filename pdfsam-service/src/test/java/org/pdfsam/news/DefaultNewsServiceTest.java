/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26 ott 2015
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
package org.pdfsam.news;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.AppBrand;
import org.pdfsam.BrandableProperty;

/**
 * @author Andrea Vacondio
 *
 */
public class DefaultNewsServiceTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private DefaultNewsService victim;
    private AppBrand appBrand;

    @Before
    public void setUp() {
        appBrand = mock(AppBrand.class);
        victim = new DefaultNewsService(appBrand);
    }

    @Test
    public void testSetLatestNewsSeen() {
        victim.clear();
        assertEquals(-1, victim.getLatestNewsSeen());
        victim.setLatestNewsSeen(5);
        assertEquals(5, victim.getLatestNewsSeen());
    }

    @Test
    public void testSetLatestImportantNewsSeen() {
        victim.clear();
        assertEquals(-1, victim.getLatestImportantNewsSeen());
        victim.setLatestImportantNewsSeen(5);
        assertEquals(5, victim.getLatestImportantNewsSeen());
    }

    @Test
    public void testClear() {
        victim.setLatestNewsSeen(5);
        assertEquals(5, victim.getLatestNewsSeen());
        victim.clear();
        assertEquals(-1, victim.getLatestNewsSeen());
    }

    @Test
    public void testGetLatestNews() throws Exception {
        File file = folder.newFile();
        FileUtils.copyInputStreamToFile(getClass().getResourceAsStream("/test_news.json"), file);
        when(appBrand.property(BrandableProperty.NEWS_URL)).thenReturn(file.toURI().toString());
        List<NewsData> news = victim.getLatestNews();
        assertEquals(1, news.size());
        assertEquals("news-title", news.get(0).getTitle());
        assertEquals("news-content", news.get(0).getContent());
        assertEquals(21, news.get(0).getId());
        assertEquals("http://www.pdfsam.org/", news.get(0).getLink());
        assertNotNull(news.get(0).getDate());
        assertTrue(news.get(0).isImportant());
    }

}
